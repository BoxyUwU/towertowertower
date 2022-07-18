#![feature(array_zip)]

use bevy::prelude::*;
use bevy_prototype_lyon::{plugin::ShapePlugin, prelude::*, shapes};
use leafwing_input_manager::prelude::*;
use std::{fmt::Debug, marker::PhantomData};

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
        .add_plugin(ShapePlugin)
        .add_plugin(InputManagerPlugin::<Action>::default())
        .insert_resource(WipNavmesh {
            corners: vec![],
            edges: vec![],
            faces: vec![],
        })
        .add_startup_system(spawn_user)
        .add_system(move_camera)
        .run()
}

pub fn triangle_corner_shape() -> shapes::Rectangle {
    shapes::Rectangle {
        extents: Vec2::new(20.0, 20.0),
        origin: RectangleOrigin::Center,
    }
}

pub fn normal_triangle_corner_draw_mode() -> DrawMode {
    DrawMode::Outlined {
        fill_mode: FillMode::color(Color::CYAN),
        outline_mode: StrokeMode::new(Color::BLACK, 5.0),
    }
}
pub fn ghost_triangle_corner_draw_mode() -> DrawMode {
    DrawMode::Outlined {
        fill_mode: FillMode::color(Color::rgba(0.0, 1.0, 1.0, 0.5)),
        outline_mode: StrokeMode::new(Color::BLACK, 5.0),
    }
}

pub fn edge_draw_mode() -> DrawMode {
    DrawMode::Stroke(StrokeMode::new(Color::rgba(0.0, 1.0, 1.0, 0.5), 5.0))
}

#[derive(Actionlike, PartialEq, Eq, Clone, Copy, Hash, Debug)]
enum Action {
    MoveLeft,
    MoveRight,
    MoveUp,
    MoveDown,

    PlaceCorner,
    UndoCorner,
    SelectCorner,
}

#[derive(Component)]
struct User;

struct KindedEntity<T>(Entity, PhantomData<T>);
impl<T> KindedEntity<T> {
    fn new(e: Entity) -> Self {
        Self(e, PhantomData)
    }
}
impl<T> Copy for KindedEntity<T> {}
impl<T> Clone for KindedEntity<T> {
    fn clone(&self) -> Self {
        Self(self.0, PhantomData)
    }
}
impl<T> Debug for KindedEntity<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Debug, Component)]
enum UserState {
    NoCorner,
    GhostCorner(Vec<(KindedEntity<Corner>, KindedEntity<Edge>)>),
}

struct UserEntityId(Entity);

fn spawn_user(mut cmds: Commands<'_, '_>) {
    let input_map = InputMap::new([
        (Action::PlaceCorner, GamepadButtonType::South),
        (Action::SelectCorner, GamepadButtonType::West),
        (Action::UndoCorner, GamepadButtonType::East),
    ]);

    let id = cmds
        .spawn()
        .insert(User)
        .insert(UserState::NoCorner)
        .insert_bundle(InputManagerBundle::<Action> {
            action_state: ActionState::default(),
            input_map,
        })
        .insert_bundle(OrthographicCameraBundle::new_2d())
        .id();

    cmds.insert_resource(UserEntityId(id));
}

struct WipNavmesh {
    faces: Vec<(
        KindedEntity<Face>,
        [(KindedEntity<Edge>, usize); 3],
        [(KindedEntity<Corner>, usize); 3],
    )>,

    edges: Vec<(
        KindedEntity<Edge>,
        // 1..=2 elements
        Vec<(KindedEntity<Face>, usize)>,
        [(KindedEntity<Corner>, usize); 2],
    )>,

    corners: Vec<(
        KindedEntity<Corner>,
        Vec<(KindedEntity<Edge>, usize)>,
        Vec<(KindedEntity<Face>, usize)>,
    )>,
}

impl WipNavmesh {
    fn new_triangle(
        &mut self,
        // dear god bevy just get query joins already -_-
        mut get_transform: impl FnMut(KindedEntity<Corner>) -> Transform,
        corners: [KindedEntity<Corner>; 3],
        edges: [KindedEntity<Edge>; 3],
        face: KindedEntity<Face>,
    ) {
        let mut get_transform = |entity| {
            let trans = get_transform(entity).translation;
            Vec2::new(trans.x, trans.y)
        };
        let corner_positions = corners.map(|corner| get_transform(corner));
        let a_to_b = corner_positions[1] - corner_positions[0];
        let a_to_c = corner_positions[2] - corner_positions[0];
        let wound_correctly = a_to_b
            .extend(0.0)
            .cross(a_to_c.extend(0.0))
            .z // FIXME zero(?)
            .is_sign_positive();

        let (corners, edges) = match wound_correctly {
            true => (corners, edges),
            false => (
                [corners[0], corners[2], corners[1]],
                [edges[0], edges[2], edges[1]],
            ),
        };

        enum Index {
            Present(usize),
            ToBeInserted(usize),
        }
        impl Index {
            fn inner(&self) -> usize {
                match self {
                    Self::Present(n) | Self::ToBeInserted(n) => *n,
                }
            }
        }

        let mut a = 0;
        let corner_indices = corners.map(|corner| {
            self.corners
                .iter()
                .enumerate()
                .flat_map(
                    |(n, (corner_entity, _, _))| match corner.0 == corner_entity.0 {
                        true => Some(Index::Present(n)),
                        false => None,
                    },
                )
                .next()
                .unwrap_or_else(|| {
                    a += 1;
                    Index::ToBeInserted(self.corners.len() + a - 1)
                })
        });
        let corners = [0, 1, 2].map(|idx| match corner_indices[idx] {
            Index::Present(n) => self.corners[n].0,
            _ => corners[idx],
        });

        let mut a = 0;
        let edge_indices = [(0, 1), (1, 2), (2, 0)].map(|(c1, c2)| {
            let (c1, c2) = (corner_indices[c1].inner(), corner_indices[c2].inner());
            self.edges
                .iter()
                .enumerate()
                .flat_map(|(n, &(_, _, [(_, other_c1), (_, other_c2)]))| {
                    match [c1, c2] == [other_c1, other_c2] || [c2, c1] == [other_c1, other_c2] {
                        true => Some(Index::Present(n)),
                        false => None,
                    }
                })
                .next()
                .unwrap_or_else(|| {
                    a += 1;
                    Index::ToBeInserted(self.edges.len() + a - 1)
                })
        });
        let edges = [0, 1, 2].map(|idx| match edge_indices[idx] {
            Index::Present(n) => self.edges[n].0,
            _ => edges[idx],
        });

        let face_index = self
            .faces
            .iter()
            .enumerate()
            .find_map(
                |(n, &(_, _, [(_, other_c1), (_, other_c2), (_, other_c3)]))| {
                    let (c1, c2, c3) = (
                        corner_indices[0].inner(),
                        corner_indices[1].inner(),
                        corner_indices[2].inner(),
                    );
                    let other_corners = [other_c1, other_c2, other_c3];
                    let same_face = [c1, c2, c3] == other_corners
                        || [c3, c1, c2] == other_corners
                        || [c2, c3, c1] == other_corners;
                    match same_face {
                        true => Some(Index::Present(n)),
                        false => None,
                    }
                },
            )
            .unwrap_or_else(|| Index::ToBeInserted(self.faces.len()));
        if let Index::Present(_) = face_index {
            return;
        }

        for (n, corner) in corner_indices.iter().enumerate() {
            if let Index::ToBeInserted(_) = corner {
                self.corners.push((corners[n], vec![], vec![]));
            };

            let idx = corner.inner();
            let corner = &mut self.corners[idx];
            corner.2.push((face, face_index.inner()));
            let n_plus_1 = match n {
                2 => 0,
                _ => n + 1,
            };
            for edge_idx in [n, n_plus_1] {
                if let Index::ToBeInserted(_) = edge_indices[edge_idx] {
                    corner
                        .1
                        .push((edges[edge_idx], edge_indices[edge_idx].inner()));
                }
            }
        }

        for (n, edge) in edge_indices.iter().enumerate() {
            if let Index::ToBeInserted(_) = edge {
                let n_plus_1 = match n {
                    2 => 0,
                    _ => n + 1,
                };

                self.edges.push((
                    edges[n],
                    vec![],
                    [
                        (corners[n], corner_indices[n].inner()),
                        (corners[n_plus_1], corner_indices[n_plus_1].inner()),
                    ],
                ))
            }

            self.edges[edge.inner()].1.push((face, face_index.inner()));
        }

        self.faces.push((
            face,
            edges.zip(edge_indices.map(|idx| idx.inner())),
            corners.zip(corner_indices.map(|idx| idx.inner())),
        ));
    }
}

#[derive(Component)]
struct Corner;
#[derive(Component)]
struct Edge;
#[derive(Component)]
struct Face;

fn new_ghost_corner(cmds: &mut Commands<'_, '_>, transform: Transform) -> Entity {
    cmds.spawn_bundle(GeometryBuilder::build_as(
        &triangle_corner_shape(),
        ghost_triangle_corner_draw_mode(),
        transform,
    ))
    .insert(Corner)
    .id()
}

fn new_edge(cmds: &mut Commands<'_, '_>, start: Vec2, end: Vec2) -> Entity {
    cmds.spawn_bundle(GeometryBuilder::build_as(
        &shapes::Line(start, end),
        edge_draw_mode(),
        Transform::from_translation(Vec3::new(0.0, 0.0, 0.0)),
    ))
    .id()
}

fn update_edge(
    triangles: &mut Query<
        (
            Entity,
            &mut Transform,
            &mut DrawMode,
            &mut Path,
            Option<With<Corner>>,
        ),
        Without<Camera>,
    >,
    edge: KindedEntity<Edge>,
    start_tri: KindedEntity<Corner>,
    end_tri: KindedEntity<Corner>,
) {
    let start_trans = triangles.get_mut(start_tri.0).unwrap().1;
    let start = Vec2::new(start_trans.translation.x, start_trans.translation.y);
    let end_transform = triangles.get_mut(end_tri.0).unwrap().1;
    let end = Vec2::new(end_transform.translation.x, end_transform.translation.y);
    let (_, _, _, mut path, _) = triangles.get_mut(edge.0).unwrap();
    *path = ShapePath::build_as(&shapes::Line(start, end));
}

fn new_triangle(
    cmds: &mut Commands<'_, '_>,
    triangles: &mut Query<
        (
            Entity,
            &mut Transform,
            &mut DrawMode,
            &mut Path,
            Option<With<Corner>>,
        ),
        Without<Camera>,
    >,
    e1: KindedEntity<Corner>,
    e2: KindedEntity<Corner>,
    e3: KindedEntity<Corner>,
) -> KindedEntity<Face> {
    let [(_, p1, ..), (_, p2, ..), (_, p3, ..)] = triangles.many_mut([e1.0, e2.0, e3.0]);

    KindedEntity::new(
        cmds.spawn_bundle(GeometryBuilder::build_as(
            &shapes::Polygon {
                points: vec![
                    Vec2::new(p1.translation.x, p1.translation.y),
                    Vec2::new(p2.translation.x, p2.translation.y),
                    Vec2::new(p3.translation.x, p3.translation.y),
                ],
                closed: true,
            },
            DrawMode::Fill(FillMode::color(Color::rgba(0.0, 1.0, 1.0, 0.5))),
            Transform::default(),
        ))
        .id(),
    )
}

fn move_camera(
    mut cmds: Commands<'_, '_>,
    mut user: Query<
        (
            &mut Transform,
            &mut UserState,
            &mut ActionState<Action>,
            &mut InputMap<Action>,
        ),
        With<Camera>,
    >,
    mut triangles: Query<
        (
            Entity,
            &mut Transform,
            &mut DrawMode,
            &mut Path,
            Option<With<Corner>>,
        ),
        Without<Camera>,
    >,
    pads: Res<Gamepads>,
    axes: Res<Axis<GamepadAxis>>,
    mut navmesh: ResMut<WipNavmesh>,
) {
    let (mut transform, mut user_state, mut actions, mut map) = user.single_mut();
    let pad = *pads.iter().next().unwrap();
    map.set_gamepad(pad);

    let x = axes
        .get(GamepadAxis(pad, GamepadAxisType::LeftStickX))
        .unwrap();
    let y = axes
        .get(GamepadAxis(pad, GamepadAxisType::LeftStickY))
        .unwrap();
    transform.translation += Vec3::new(x, y, 0.0) * 25.0;

    if let UserState::GhostCorner(corners) = &*user_state {
        let (corner, _) = *corners.last().unwrap();
        triangles.get_mut(corner.0).unwrap().1.translation = transform.translation;
    }

    if actions.just_pressed(Action::SelectCorner) {
        let to_select = triangles
            .iter_mut()
            .flat_map(|(e, pos, _, _, corner)| {
                corner.map(|_| {
                    (
                        e,
                        *pos,
                        (Vec2::new(transform.translation.x, transform.translation.y)
                            - Vec2::new(pos.translation.x, pos.translation.y))
                        .length()
                        .abs(),
                    )
                })
            })
            .fold(
                None::<(Entity, Transform, f32)>,
                |lowest, (e, trans, dist)| match lowest {
                    Some((_, _, lowest_dist)) if 0.1 <= dist && dist <= lowest_dist => {
                        Some((e, trans, dist))
                    }
                    Some(_) => lowest,
                    None if 0.1 <= dist && dist <= 100.0 => Some((e, trans, dist)),
                    None => None,
                },
            );

        if let Some((e, _, _)) = to_select {
            match &mut *user_state {
                UserState::NoCorner => (),
                UserState::GhostCorner(corners) => {
                    // FIXME i think we end up creating unnecessary edges here.
                    let (ghost_corner, _) = corners.last_mut().unwrap();
                    cmds.entity(ghost_corner.0).despawn();
                    *ghost_corner = KindedEntity::new(e);
                    actions.press(Action::PlaceCorner);
                }
            }
        }
    }

    if let UserState::GhostCorner(corners) = &*user_state {
        for (start_idx, end_idx) in [(0, 1), (1, 2), (2, 0)] {
            if let (Some((start_corner, edge_entity)), Some((end_corner, _))) =
                (corners.get(start_idx), corners.get(end_idx))
            {
                update_edge(&mut triangles, *edge_entity, *start_corner, *end_corner);
            }
        }
    }

    if actions.just_pressed(Action::PlaceCorner) {
        match &mut *user_state {
            UserState::NoCorner => {
                let pos = Vec2::new(transform.translation.x, transform.translation.y);
                *user_state = UserState::GhostCorner(vec![(
                    KindedEntity::new(new_ghost_corner(&mut cmds, *transform)),
                    KindedEntity::new(new_edge(&mut cmds, pos, pos)),
                )]);
            }
            UserState::GhostCorner(corners) => {
                assert!(matches!(corners.len(), 1..=3));
                if let [(a_corner, a_edge), (b_corner, b_edge), (c_corner, c_edge)] = corners[..] {
                    let face =
                        new_triangle(&mut cmds, &mut triangles, a_corner, b_corner, c_corner);
                    navmesh.new_triangle(
                        |entity| *triangles.get_component::<Transform>(entity.0).unwrap(),
                        [a_corner, b_corner, c_corner],
                        [a_edge, b_edge, c_edge],
                        face,
                    );
                    corners.clear();
                }

                let pos = Vec2::new(transform.translation.x, transform.translation.y);
                let new_corner = (
                    KindedEntity::new(new_ghost_corner(&mut cmds, *transform)),
                    KindedEntity::new(new_edge(&mut cmds, pos, pos)),
                );
                corners.push(new_corner);
            }
        }
    }
}
