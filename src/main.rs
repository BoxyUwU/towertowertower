use bevy::prelude::*;
use bevy_prototype_lyon::{plugin::ShapePlugin, prelude::*, shapes};
use leafwing_input_manager::prelude::*;

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

#[derive(Component)]
enum UserState {
    NoCorner,
    GhostCorner {
        /// Vec<{ triangle_entity: Entity, edge_entity_to_ghost_corner: Entity }>
        already_placed: Vec<(Entity, Entity)>,
        ghost_corner: Entity,
    },
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

struct WipNavmesh(Vec<[Entity; 3]>);

#[derive(Component)]
struct Corner;

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
        &shapes::Line(Vec2::new(0.0, 0.0), end - start),
        edge_draw_mode(),
        Transform::from_translation(Vec3::new(start.x, start.y, 0.0)),
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
    edge: Entity,
    start_tri: Entity,
    end_tri: Entity,
) {
    let start_trans = triangles.get_mut(start_tri).unwrap().1;
    let start = Vec2::new(start_trans.translation.x, start_trans.translation.y);
    let end_transform = triangles.get_mut(end_tri).unwrap().1;
    let end = Vec2::new(end_transform.translation.x, end_transform.translation.y);
    let (_, _, _, mut path, _) = triangles.get_mut(edge).unwrap();
    *path = ShapePath::build_as(&shapes::Line(Vec2::new(0.0, 0.0), end - start));
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
    e1: Entity,
    e2: Entity,
    e3: Entity,
) {
    let [(_, p1, ..), (_, p2, ..), (_, p3, ..)] = triangles.many_mut([e1, e2, e3]);

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
    ));
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

    if let UserState::GhostCorner {
        already_placed,
        ghost_corner,
    } = &mut *user_state
    {
        triangles.get_mut(*ghost_corner).unwrap().1.translation = transform.translation;
        for (start_entity, edge_entity) in already_placed {
            update_edge(&mut triangles, *edge_entity, *start_entity, *ghost_corner);
        }
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
                UserState::GhostCorner {
                    already_placed: _,
                    ghost_corner,
                } => {
                    cmds.entity(*ghost_corner).despawn();
                    *ghost_corner = e;
                    actions.press(Action::PlaceCorner);
                }
            }
        }
    }

    if actions.just_pressed(Action::PlaceCorner) {
        let new_ghost_corner_id = new_ghost_corner(&mut cmds, *transform);

        match &mut *user_state {
            UserState::NoCorner => {
                *user_state = UserState::GhostCorner {
                    already_placed: vec![],
                    ghost_corner: new_ghost_corner_id,
                };
            }
            UserState::GhostCorner {
                already_placed,
                ghost_corner,
            } => {
                let (_, ghost_pos, mut draw_mode, ..) = triangles.get_mut(*ghost_corner).unwrap();
                let ghost_pos = *ghost_pos;
                *draw_mode = normal_triangle_corner_draw_mode();

                already_placed.push((
                    *ghost_corner,
                    new_edge(
                        &mut cmds,
                        Vec2::new(ghost_pos.translation.x, ghost_pos.translation.y),
                        Vec2::new(ghost_pos.translation.x, ghost_pos.translation.y),
                    ),
                ));
                let old_ghost_corner_id = *ghost_corner;
                *ghost_corner = new_ghost_corner_id;

                if let [(a, a_edge), (b, b_edge), (c, _)] = already_placed[..] {
                    update_edge(&mut triangles, a_edge, a, c);
                    update_edge(&mut triangles, b_edge, b, c);

                    new_triangle(&mut cmds, &mut triangles, a, b, c);
                    navmesh.0.push([a, b, c]);
                    already_placed.clear();
                }

                if let [(a_entity, ref mut edge_to_yeet), (_, _)] = already_placed[..] {
                    update_edge(&mut triangles, *edge_to_yeet, a_entity, old_ghost_corner_id);

                    let start_trans = *triangles.get_mut(a_entity).unwrap().1;
                    *edge_to_yeet = new_edge(
                        &mut cmds,
                        Vec2::new(start_trans.translation.x, start_trans.translation.y),
                        Vec2::new(transform.translation.x, transform.translation.y),
                    );
                }
            }
        }
    }
}
