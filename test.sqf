params ["_target", "_vehicle"];

for "_i" from 1 to 1 do {
	private _position = [_target, 20] call fn_random;
	gunner _vehicle lookAt _position;
	sleep 3;
	
	_vehicle setWeaponReloadingTime [gunner _vehicle, currentMuzzle gunner _vehicle, 0];

	_vehicle fire ([getPos player, _this] spawn { for "_i" from 1 to 1 do { private _position = [_this select 0, 20] call fn_random; gunner (_this select 1) lookAt _position; sleep 3; (_this select 1) setWeaponReloadingTime [gunner (_this select 1), (_this select 1) weaponsTurret [0], 0]; (_this select 1) fire (((_this select 1) weaponsTurret [0]) select 0); }; };) select 0);
};
