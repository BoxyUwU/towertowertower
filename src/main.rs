use bevy::prelude::*;
use bevy_prototype_lyon::{plugin::ShapePlugin, prelude::*, shapes};
use leafwing_input_manager::prelude::*;
use std::{fmt::Debug, marker::PhantomData};

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
        .add_plugin(ShapePlugin)
        .add_plugin(InputManagerPlugin::<Action>::default())
        .insert_resource(WipNavmesh(vec![]))
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

#[allow(dead_code)]
struct Triangle {
    corners: [KindedEntity<Corner>; 3],
    edges: [KindedEntity<Edge>; 3],
    face: KindedEntity<Face>,
}

struct WipNavmesh(Vec<Triangle>);

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
                if let [(a_corner, a_edge), (b_corner, b_edge), (c_corner, c_edge)] = corners[..] {
                    let face =
                        new_triangle(&mut cmds, &mut triangles, a_corner, b_corner, c_corner);
                    navmesh.0.push(Triangle {
                        corners: [a_corner, b_corner, c_corner],
                        edges: [a_edge, b_edge, c_edge],
                        face,
                    });
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
