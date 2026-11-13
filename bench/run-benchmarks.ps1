param([string]$Rscript = 'C:/Program Files/R/R-4.6.1/bin/Rscript.exe', [int]$WaitForProcess = 0,
      [string]$Packages = 'ggplot3d,ggcube', [string]$CellSizes = '50,100,200,316',
      [ValidateSet('benchmark','profile')][string]$Mode = 'benchmark')
$ErrorActionPreference = 'Stop'
if ($WaitForProcess -gt 0) {
  $predecessor = Get-Process -Id $WaitForProcess -ErrorAction SilentlyContinue
  if ($predecessor) { $predecessor.WaitForExit() }
}
$repo = [IO.Path]::GetFullPath((Join-Path $PSScriptRoot '../..'))
$runDate = (Get-Content -LiteralPath (Join-Path $PSScriptRoot 'run-date.txt')).Trim()
$out = Join-Path $PSScriptRoot ('raw-' + $runDate)
New-Item -ItemType Directory -Force -Path $out | Out-Null
# A Windows job object limits the committed memory of each R process.
if (-not ('BenchMemoryJob' -as [type])) {
Add-Type -TypeDefinition @'
using System;
using System.Runtime.InteropServices;
public class BenchMemoryJob : IDisposable {
  [StructLayout(LayoutKind.Sequential)] struct Basic {
    public long a,b; public uint flags; public UIntPtr min,max;
    public uint active; public UIntPtr affinity; public uint priority,scheduling;
  }
  [StructLayout(LayoutKind.Sequential)] struct Io { public ulong a,b,c,d,e,f; }
  [StructLayout(LayoutKind.Sequential)] struct Extended {
    public Basic basic; public Io io; public UIntPtr process,job,peakProcess,peakJob;
  }
  [DllImport("kernel32.dll",CharSet=CharSet.Unicode)] static extern IntPtr CreateJobObject(IntPtr a,string b);
  [DllImport("kernel32.dll",SetLastError=true)] static extern bool SetInformationJobObject(IntPtr a,int b,IntPtr c,uint d);
  [DllImport("kernel32.dll",SetLastError=true)] static extern bool AssignProcessToJobObject(IntPtr a,IntPtr b);
  [DllImport("kernel32.dll",SetLastError=true)] static extern bool QueryInformationJobObject(IntPtr a,int b,IntPtr c,uint d,IntPtr e);
  [DllImport("kernel32.dll")] static extern bool CloseHandle(IntPtr a);
  IntPtr handle;
  public BenchMemoryJob(ulong bytes) {
    handle=CreateJobObject(IntPtr.Zero,null);
    var limits=new Extended(); limits.basic.flags=0x100|0x200|0x2000;
    limits.process=new UIntPtr(bytes);
    limits.job=new UIntPtr(bytes);
    int size=Marshal.SizeOf(limits); IntPtr memory=Marshal.AllocHGlobal(size);
    try {
      Marshal.StructureToPtr(limits,memory,false);
      if(!SetInformationJobObject(handle,9,memory,(uint)size)) throw new System.ComponentModel.Win32Exception();
    } finally { Marshal.FreeHGlobal(memory); }
  }
  public void Assign(IntPtr process) {
    if(!AssignProcessToJobObject(handle,process)) throw new System.ComponentModel.Win32Exception();
  }
  public ulong PeakBytes() {
    int size=Marshal.SizeOf(typeof(Extended)); IntPtr memory=Marshal.AllocHGlobal(size);
    try {
      if(!QueryInformationJobObject(handle,9,memory,(uint)size,IntPtr.Zero)) throw new System.ComponentModel.Win32Exception();
      return ((Extended)Marshal.PtrToStructure(memory,typeof(Extended))).peakJob.ToUInt64();
    } finally { Marshal.FreeHGlobal(memory); }
  }
  public void Dispose() { if(handle!=IntPtr.Zero) {CloseHandle(handle);handle=IntPtr.Zero;} }
}
'@
}
Get-CimInstance Win32_Processor | Select-Object Name,NumberOfCores,NumberOfLogicalProcessors |
  Format-List | Out-File (Join-Path $out 'hardware.txt')
Get-CimInstance Win32_OperatingSystem | Select-Object Caption,Version,TotalVisibleMemorySize |
  Format-List | Out-File (Join-Path $out 'hardware.txt') -Append
foreach ($cells in ($CellSizes.Split(',') | ForEach-Object { [int]$_ })) {
  foreach ($package in $Packages.Split(',')) {
    if ($package -notin @('ggplot3d','ggcube')) { throw "Unsupported benchmark package $package" }
    $stem = Join-Path $out "$package-$cells"
    $runStem = if($Mode -eq 'profile') { "$stem-profile-run" } else { $stem }
    foreach($suffix in @('-error.txt','-exit.txt','-done.txt')) {
      if(Test-Path -LiteralPath "$runStem$suffix") { Remove-Item -LiteralPath "$runStem$suffix" }
    }
    $snapshot = Join-Path $out "worker-$PID-$package-$cells-$Mode.R"
    Copy-Item -LiteralPath (Join-Path $PSScriptRoot 'worker.R') -Destination $snapshot
    Get-ChildItem -LiteralPath (Join-Path $repo 'R') -Filter '*.R' -File |
      Get-FileHash -Algorithm SHA256 | Select-Object Path,Hash |
      Export-Csv -NoTypeInformation -LiteralPath "$runStem-R-source.csv"
    $job = [BenchMemoryJob]::new([uint64](4GB - 1MB))
    try {
      $process = Start-Process -FilePath $Rscript -ArgumentList @($snapshot,$package,$cells,$stem,$Mode) `
        -WorkingDirectory $repo -WindowStyle Hidden -PassThru `
        -RedirectStandardOutput "$runStem-stdout.log" -RedirectStandardError "$runStem-stderr.log"
      $job.Assign($process.Handle)
      $peak = [int64]0
      while (!$process.WaitForExit(1000)) {
        $process.Refresh()
        $peak = [Math]::Max($peak,[int64]$job.PeakBytes())
        Set-Content -LiteralPath "$runStem-memory.txt" -Value $peak
      }
      $process.Refresh()
      $peak = [Math]::Max($peak,[int64]$job.PeakBytes())
      Set-Content -LiteralPath "$runStem-memory.txt" -Value $peak
      Set-Content -LiteralPath "$runStem-exit.txt" -Value $process.ExitCode
    } finally { $job.Dispose() }
  }
}
& $Rscript bench/summarise.R $out
