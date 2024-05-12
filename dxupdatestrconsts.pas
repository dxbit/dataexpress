unit DXUpdateStrConsts;

{$mode objfpc}{$H+}

interface

resourcestring
  rsUpdating = 'Updating DataExpress...';
  rsError = 'Error';
  rsWarning = 'Warning';
  rsStartUpdateError = 'Unable to start DataExpress update. File %s is in use '
    +'by another process. Make sure you don''t have any instances of '
    +'DataExpress running. Close all applications that may interfere with the '
    +'update process.';
  rsCopyFileError = 'Can not copy file %s into %s.';
  rsFullErrMsg = '%0:s\n\nException class: %1:s\nException message: %2:s';
  rsUpdateAbortedUser = 'The update was aborted by the user.';
  rsRetry = 'Retry';
  rsAbort = 'Abort';
  //rsCantCreateFolder = 'Can''t create folder %s. Please check the permissions '
  //  +'of the folder %s.';
  //rsRunDXAgain = 'Remove the cause of the error and run DataExpress again.';

implementation

end.

