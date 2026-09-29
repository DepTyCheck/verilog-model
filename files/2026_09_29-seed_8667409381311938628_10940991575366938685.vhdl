-- Seed: 8667409381311938628,10940991575366938685

entity mdzgp is
  port (htp : linkage time; qmcpbhl : inout real);
end mdzgp;

architecture lnt of mdzgp is
  
begin
  
end lnt;

entity yfrbb is
  port (t : out string(1 to 5); bpko : out character);
end yfrbb;

architecture iafdcey of yfrbb is
  
begin
  -- Single-driven assignments
  bpko <= 'b';
  t <= ('t', 'e', 'j', 'v', 'z');
end iafdcey;

entity vfbmxlitcj is
  port (k : in real; iyvz : inout time; nkwtk : inout character);
end vfbmxlitcj;

architecture hsileor of vfbmxlitcj is
  signal h : real;
  signal qrxop : real;
  signal twmof : time;
begin
  vms : entity work.mdzgp
    port map (htp => twmof, qmcpbhl => qrxop);
  fqsjr : entity work.mdzgp
    port map (htp => iyvz, qmcpbhl => h);
  
  -- Single-driven assignments
  nkwtk <= nkwtk;
end hsileor;

entity ekn is
  port (avuf : inout time; rqe : out boolean_vector(2 to 4); emwh : out time);
end ekn;

architecture amhuv of ekn is
  signal aykhv : character;
  signal o : string(1 to 5);
  signal yjtesew : character;
  signal vs : string(1 to 5);
  signal qta : character;
  signal uwxpg : real;
begin
  tmufknr : entity work.vfbmxlitcj
    port map (k => uwxpg, iyvz => avuf, nkwtk => qta);
  hfr : entity work.yfrbb
    port map (t => vs, bpko => yjtesew);
  ar : entity work.yfrbb
    port map (t => o, bpko => aykhv);
  pincpczq : entity work.mdzgp
    port map (htp => emwh, qmcpbhl => uwxpg);
  
  -- Single-driven assignments
  rqe <= (FALSE, TRUE, FALSE);
end amhuv;



-- Seed after: 1691998908724211375,10940991575366938685
