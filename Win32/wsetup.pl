:- module(wsetup,[main/1],[assertions,isomodes]).
% This, for making it a user file when debugging:
% :- use_package([assertions,isomodes]).

% --------------------------------------------------------------------------
:- comment(title,"Setup Script for Ciao Installation under Windows").
% --------------------------------------------------------------------------

:- comment(module,"This module performs the installation of Ciao under
   Windows. 

   If called with no argument, or with an argument @tt{main} it performs 
   the following actions:

   @begin{itemize}

   @item Create the default compiler header and prototype @tt{.bat}
         files for executables.

   @item Build the info index (@file{dir}) in
         @file{SRC/doc/reference}, the setup script
         (@file{DOTemacs.el}) in @file{SRC} and the @apl{emacs} mode
         (@file{ciao.el}) in @file{SRC/emacs}.

   @item Create a file @tt{ciao.reg} suitable for entering the
         Ciao/Prolog-related associations into the Windows registry
         using @apl{regedit}.

   @end{itemize}

   If called with a single argument @tt{client} it performs only the
   creation of the registry update file (in
   @tt{c:/WINDOWS/TEMP/ciaocl.reg}). This is useful for updating the
   registry of the clients (which typically cannot write into the
   shared areas where Ciao would be installed) in a server/client
   setup.

   ").

:- comment(author,"Daniel Cabeza").
:- comment(author,"Manuel Hermenegildo").

% --------------------------------------------------------------------------

:- use_module(library(lists), [append/3,list_concat/2]).
:- use_module(library(streams), [open_output/2, close_output/1]).
:- use_module(library(system), [getenvstr/2, working_directory/2,cd/1]).
:- use_module(library('make/system_extra'),
	[cat/2,delete_file/1,readf/2,replace_strings_in_file/3,
	 cyg2win/3,writef/3]).
:- use_module(registry,[win_reg/3]).

% Extension used for executables in Windows:
:- include(library('compiler/win_exec_ext')).

main([]) :-
	!,
	main([main]).
main([T]) :-
	!,
	setup_mess(['                                           \n',
		    '*    **** Ciao Windows ',T,' installation ****    \n',
                    '*                                           \n']),

        working_directory(SDir,SDir), % SRC
        display(SDir),nl,
        atom_codes(SDir,SDirStr),
        cyg2win(SDirStr,CiaoPath,swap),
        display_string(CiaoPath),nl,
        append(CiaoPath,"\\Win32\\bin\\ciaoengine.exe",EnginePath),
        list_concat(["""",EnginePath,""""],EngineQuot),
        cyg2win(SDirStr,SRCS,noswap),
        atom_codes(SRC,SRCS),

	(  T == main
        -> make_infoindex(SRC,IDir),
	   make_DOTemacs(SRC,IDir,EDir),
	   make_ciaomode(SRC,EDir),

           make_header(SDir),
           make_bats(EngineQuot),
           make_foremacs(SRC),

           CReg = 'ciao.reg'
        ;  CReg =  'c:/WINDOWS/TEMP/ciaocl.reg' ),

	setup_mess(['Building Win register file ',CReg,'.\n']),
        ( getenvstr('OS',"Windows_NT") -> % Also for Windows 2000
            Engine = EnginePath
        ; Engine = EngineQuot
        ),
        exec_ext(ExeExt),
        ciaoreg(CiaoPath, Engine, ExeExt, Registry),
        win_reg(Registry, String, []),
        open_output(CReg,Out),
        display_string(String),
        close_output(Out),

	setup_mess([
  '                                           \n',
  '*    **** Installation Complete ****       \n',
  '*                                          \n',
  '*    You may need to reboot for the changes in the registry take effect\n',
  '*                                          \n']),
	line,
% Done in bat, after regedit...
%	display('Hit any key to exit... '),flush_output,
%	get_code(_),
	true.
main(Args) :-
	display_list(['ERROR: illegal arguments '|Args]).

make_infoindex(SDir,IDir) :-
	atom_concat(SDir,'/doc/reference',IDir),
	cd(IDir),
	setup_mess(['Building ',IDir,'/dir (info index).\n']),
	cat(['Indexhead_ciao.info','ciao.infoindex','Indextail_ciao.info'],
	    dir),
	cd(SDir).

make_DOTemacs(SDir,IDir,EDir) :-
	atom_concat(SDir,'/emacs-mode',EDir),
	cd(EDir),
	setup_mess(['Building ',SDir,'/DOTemacs.el (emacs setup).\n']),
	atom_codes(EDir,EDirS),
	atom_codes(IDir,IDirS),
	replace_strings_in_file([ "<CIAOLIBDIR>" - EDirS, 
                                  "<LPDOCDIR>" - IDirS],
                                'DOTemacs.skel','../DOTemacs.el'),
        atom_codes(SDir,SDirS),
	list_concat([
	   ";; Specific to Windows installation:\n",
	   ";; Location of Ciao shell\n",
	   "(setq ciao-system (convert-standard-filename \n",
	   "      \"",SDirS,"/shell/ciaosh.bat\"))\n",
	   ";; Location of info manuals\n",
	   "(setq Info-default-directory-list  (cons \n",
	   "      \"",SDirS,"/doc/reference\" \n",
	   "      Info-default-directory-list))\n",
           %%% Should put this in a separate file in SRC/emacs-mode and cat it now:
           ";; Make things nicer (but check if you are already doing it)\n",
	   "(global-font-lock-mode)\n",
	   "(transient-mark-mode t)\n",
	   ";; Help for using the Windows command.com as your shell\n",
	   ";; (comment out if you use bash, etc.):\n",
	   "(setq process-coding-system-alist
	    '((\"cmdproxy\" . (raw-text-dos . raw-text-dos))))\n",
	   ";; Preventing ctrln-m's from being printed in the shell\n",
	   "(add-hook 'comint-output-filter-functions ",
	   "  'shell-strip-ctrl-m nil t)\n",
	   "; -----",
	   "----------------------------------------------------------------\n"
		    ],NewLisp),
	writef(NewLisp,append,'../DOTemacs.el'),
	cd(SDir).

make_foremacs(SDir):-
	atom_codes(SDir, SDirS),
        list_concat([
                        ";; Include this line in your ~/.emacs file:\n",
                        "(load-file \"", SDirS,  "/DOTemacs.el\")\n"],
                        ForEmacs),
        cd(SDir),
        writef(ForEmacs, write, 'ForEmacs.txt').

make_ciaomode(SDir,EDir) :-
	cd(EDir),
	setup_mess(['Building ',EDir,'/ciao.el (emacs mode).\n']),
	atom_codes(EDir,EDirS),
	replace_strings_in_file([ "\n" - "\n;" ],
                                '../DOTemacs.el','DOTemacs.tmp'),
	cat(['ciao.el.header','DOTemacs.tmp','ciao.el.body'],'ciao.el.tmp'),
	delete_file('DOTemacs.tmp'),
	replace_strings_in_file([ "<CIAOREALLIBDIR>" - EDirS ],
                                'ciao.el.tmp','ciao.el'),
        delete_file('ciao.el.tmp'),
	cd(SDir).

make_header(CiaoPath) :-
	setup_mess(['Building header to ',CiaoPath,
                    '/Win32/bin/ciaoengine.exe.\n']),
%        open_output('$/lib/compiler/header', Out),
        atom_concat(CiaoPath, '/lib/compiler/header', HeaderPath),
        open_output(HeaderPath, Out),
        display('#!/bin/sh\n'),
        display('INSTENGINE="'),
        display(CiaoPath),
        display('/Win32/bin/ciaoengine.exe"\n'),
        display('ENGINE=${CIAOENGINE:-${INSTENGINE}}\n'),
        display('exec "$ENGINE" "$@" -C -b $0\n\^L\n'),
        close_output(Out).

make_bats(Engine) :-
	setup_mess(['Building prototype .bat files pointing to engine.\n']),
        ( getenvstr('OS',"Windows_NT") ->
            AllArgs = ' %*' 
        ; AllArgs = ' %1 %2 %3 %4 %5 %6 %7 %8 %9'
        ),
        bat_file(BatFile, Head, Tail),
          open_output(BatFile, Out),
          display(Head),
          display_string("@"||Engine),
          display(AllArgs),
          display(Tail),
          close_output(Out),
        fail.
make_bats(_).

bat_file('Win32/bat_skel',
         '@REM Change /path/to/ciao/application to the absolute path \c
          of the application\n',
         ' -C -b "/path/to/ciao/application"').
bat_file('shell/ciaosh.bat','',' -C -i -b "$/shell/ciaosh"').
bat_file('ciaoc/ciaoc.bat','',' -C -b "$/ciaoc/ciaoc"').

:- pred ciaoreg(+string, +string, +atm, -string).

ciaoreg(CiaoPath, Engine, ExeExt, Reg) :-
        append(CiaoPath, "\\Win32\\ciaoexe.ico,0", ExeIco),
        append(CiaoPath, "\\Win32\\ciaoitf.ico,0", ItfIco),
        append(CiaoPath, "\\Win32\\ciaopo.ico,0", PoIco),
        append(CiaoPath, "\\Win32\\ciaoasr.ico,0", AsrIco),
        append(CiaoPath, "\\Win32\\ciaopl.ico,0", PlIco),
        append(CiaoPath, "\\Win32\\ciaoscrt.ico,0", ScrtIco),
        append(Engine, " %2 %3 %4 %5 %6 %7 %8 %9 -C -b ""%1""",
               ExeCommand),
        append(Engine," -u ""%1"" -C -i -b $\\shell\\ciaosh", LoadFileCmd),
        append(Engine," ""%1"" -C -b $\\ciaoc\\ciaoc", MakeExecCmd),
        append(Engine,
               " ""%1"" %2 %3 %4 %5 %6 %7 %8 %9 -C -b $\\shell\\ciao-shell",
               ExeScrtCmd),
        append(Engine, " -C -b %s %s",IIS_string),
        ROOT = 'HKEY_CLASSES_ROOT',
        Reg = [
        % .cpx files
        [[ROOT,ExeExt],
          '@'="ciaoexefile"],
        [[ROOT,ciaoexefile],
          '@'="Ciao executable"%,
         % 'EditFlags'=0x00000000
        ],
        [[ROOT,ciaoexefile,'DefaultIcon'],
          '@'=ExeIco],
        [[ROOT,ciaoexefile,shell],
          '@'=""],
        [[ROOT,ciaoexefile,shell,open],
          'EditFlags'=0x01000000],
        [[ROOT,ciaoexefile,shell,open,command],
          '@'=ExeCommand],
        % .pl files
        [[ROOT,'.pl'],
          '@'="ciaofile"],
        [[ROOT,ciaofile],
          '@'="Ciao Prolog source",
          'EditFlags'=0x00000000],
        [[ROOT,ciaofile,'DefaultIcon'],
          '@'=PlIco],
        [[ROOT,ciaofile,shell],
          '@'=""],
        [[ROOT,ciaofile,shell,open],
          'EditFlags'=0x01000000],
        [[ROOT,ciaofile,shell,open,command],
          '@'=""],
        [[ROOT,ciaofile,shell,load_file],
          '@'="Load into Toplevel shell"],
        [[ROOT,ciaofile,shell,load_file,command],
          '@'=LoadFileCmd],
        [[ROOT,ciaofile,shell,make_executable],
          '@'="Make executable"],
        [[ROOT,ciaofile,shell,make_executable,command],
          '@'=MakeExecCmd],
        % .pls files
        [[ROOT,'.pls'],
          '@'="ciaoplsfile"],
        [[ROOT,ciaoplsfile],
          '@'="Ciao Prolog script",
          'EditFlags'=0x00000000],
        [[ROOT,ciaoplsfile,'DefaultIcon'],
          '@'=ScrtIco],
        [[ROOT,ciaoplsfile,shell],
          '@'=""],
        [[ROOT,ciaoplsfile,shell,open],
          'EditFlags'=0x01000000],
        [[ROOT,ciaoplsfile,shell,open,command],
          '@'=ExeScrtCmd],
        % .itf files
        [[ROOT,'.itf'],
          '@'="ciaoitffile"],
        [[ROOT,ciaoitffile],
          '@'="Ciao interface file",
          'EditFlags'=0x00000000],
        [[ROOT,ciaoitffile,'DefaultIcon'],
          '@'=ItfIco],
        [[ROOT,ciaoitffile,shell],
          '@'=""],
        % .po files
        [[ROOT,'.po'],
          '@'="ciaopofile"],
        [[ROOT,ciaopofile],
          '@'="Ciao object file",
          'EditFlags'=0x00000000],
        [[ROOT,ciaopofile,'DefaultIcon'],
          '@'=PoIco],
        [[ROOT,ciaopofile,shell],
          '@'=""],
        % .asr files
        [[ROOT,'.asr'],
          '@'="ciaoasrfile"],
        [[ROOT,ciaoasrfile],
          '@'="Ciao assertions file",
          'EditFlags'=0x00000000],
        [[ROOT,ciaoasrfile,'DefaultIcon'],
          '@'=AsrIco],
        [[ROOT,ciaoasrfile,shell],
          '@'=""],
	[['HKEY_LOCAL_MACHINE','SOFTWARE','Ciao Prolog'],
	 ciao_dir=CiaoPath],
        % For Microsoft's IIS
        [['HKEY_LOCAL_MACHINE','SYSTEM','CurrentControlSet',
          'Services','W3SVC','Parameters','Script Map'],
          ExeExt=IIS_string]
        ].
        
% --------------------------------------------------------------------------
% Utilities
% --------------------------------------------------------------------------

setup_mess(M) :-
	line,
	display_list(['* '|M]),
	flush_output.

line :- 
	display(
'--------------------------------------------------------------------------\n'
               ).

% --------------------------------------------------------------------------
:- comment(version_maintenance,dir('../version')).

:- comment(version(1*7+111,2001/06/20,18:58*51+'CEST'), "Added an entry
   to the Windows registry to allow loading a file into a new toplevel
   by right-clicking on a .pl file.  (Daniel Cabeza Gras)").

:- comment(version(1*7+66,2001/03/15,20:47*55+'CET'), "The installation
   program in Windows now adds an entry in the registry for running Ciao
   executables as CGIs under IIS.  (Daniel Cabeza Gras)").

:- comment(version(1*5+168,2000/07/05,16:24*21+'CEST'), "ForEmacs.txt
        was not being created; corrected.  (MCL)").

:- comment(version(1*5+166,2000/06/27,16:03*35+'CEST'), "Changed to
   support Windows 2000 (with the NT registry options).  (Daniel Cabeza
   Gras)").

:- comment(version(1*3+103,1999/11/17,11:18*32+'MET'), "Moved most of
   the utilities to @lib{system_extra}.  (Manuel Hermenegildo)").

:- comment(version(1*3+102,1999/11/15,17:04*55+'MET'), "Added Windows
   setup script to version control.  (Manuel Hermenegildo)").
% --------------------------------------------------------------------------
