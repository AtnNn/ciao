% Manifest file for Ciao (engine, compiler, basic libraries, extra
% libraries, and contributed code)
bundle_name(ciao).
bundle_packname('Ciao').
bundle_type(basic).
src_alias_paths([ciaosrc = '.']).
ins_alias_paths([
		rtchecks = 'lib/rtchecks',
		unittest = 'lib/unittest',
		plindent = 'contrib/plindent',
		predefres = 'lib/resdefs/predefres',
		res_nargs = 'lib/resdefs/predefres/res_nargs',
		res_steps = 'lib/resdefs/predefres/res_steps',
		predefprf = 'lib/resdefs/predefprf',
		prf_ticks = 'lib/resdefs/predefprf/prf_ticks',
		prf_costcenter = 'lib/resdefs/predefprf/prf_costcenter'
	    ]).
