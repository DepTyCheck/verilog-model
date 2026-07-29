-- Seed: 9039580398187188787,14641901754878719179

entity vdp is
  port (rdov : buffer time);
end vdp;

architecture rhysfoc of vdp is
  
begin
  -- Single-driven assignments
  rdov <= rdov;
end rhysfoc;

entity qm is
  port (y : linkage integer; wybkto : buffer severity_level);
end qm;

architecture ixlcxeipzs of qm is
  signal lh : time;
  signal ubfnb : time;
  signal okhcola : time;
begin
  ilc : entity work.vdp
    port map (rdov => okhcola);
  syafajqxj : entity work.vdp
    port map (rdov => ubfnb);
  covhs : entity work.vdp
    port map (rdov => lh);
  
  -- Single-driven assignments
  wybkto <= WARNING;
end ixlcxeipzs;



-- Seed after: 4821879662950298355,14641901754878719179
