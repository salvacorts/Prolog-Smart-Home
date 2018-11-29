%%%%% The Database of Facts to be Queried

%% location(Location): Defines a location (room)
location(living_room).
location(kitchen).
location(corridor).
location(bedroom_1).
location(bedroom_2).

%% object(Object): Defines an object
object(light).
object(heating).
object(ac).
object(curtain).
object(tv).
object(speaker).
object(oven).
object(cofee_machine).
object(dish_washer).

%% equiped(Location, Object): Location has the Object
equiped(living_room, light).
equiped(living_room, heating).
equiped(living_room, ac).
equiped(living_room, curtain).
equiped(living_room, tv).
equiped(living_room, speaker).

equiped(kitchen, light).
equiped(kitchen, heating).
equiped(kitchen, oven).
equiped(kitchen, cofee_machine).
equiped(kitchen, dish_washer).

equiped(bedroom_1, light).
equiped(bedroom_1, heating).
equiped(bedroom_1, ac).
equiped(bedroom_1, curtain).
equiped(bedroom_1, tv).

equiped(bedroom_2, light).
equiped(bedroom_2, heating).
equiped(bedroom_2, curtain).

equiped(corridor, light).

%% action(Object, Action): The Object can perform an action
action(light, power). % power means it can be ON or OFF

action(curtain, power). % In this case power means closed or opened

action(heating, power).
action(heating, increase).
action(heating, decrease).

action(ac, power).
action(ac, increase).
action(ac, decrease).

action(tv, power).
action(tv, volume_up).
action(tv, volume_down).
action(tv, channel_up).
action(tv, channel_down).

action(speaker, power).
action(speaker, volume_up).
action(speaker, volume_down).
action(speaker, play).
action(speaker, pause).
% action(speaker, pause). maybe play song? but it has another argument 

action(oven, power).
action(oven, increase).
action(oven, decrease).
% action(oven, set_timer). Again we would need another argument

action(dish_washer, power).
action(dish_washer, start).

action(cofee_machine, power).
action(cofee_machine, make). % make cofee