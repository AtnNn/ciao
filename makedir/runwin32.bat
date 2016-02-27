@rem Script that is executed after windows installation

@echo Please wait while CiaoDE is configured...
@echo
%1 -C -b "build/bin/config_source_components"
@cd ciao
@echo Please verify that no errors appears in this log file > ..\environment_ciao.log 2>&1
@echo Command executed: >> ..\environment_ciao.log 2>&1
@echo %0 %1 %2 >> ..\environment_ciao.log 2>&1
@echo Output:  >> ..\environment_ciao.log 2>&1
%1 -d emacs_path=%2/emacs.exe -d install_emacs_support=yes -d emacs_type=Win32 environment windows_bats -C -b ../build/bin/lpmake.sta >> ..\environment_ciao.log 2>&1
@start wordpad ..\environment_ciao.log
cd ..
