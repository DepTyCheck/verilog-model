-- Seed: 18066807538869140526,8881251984038598933



entity dmnixm is
  port (m : in real; q : in real);
end dmnixm;



architecture fuxzy of dmnixm is
  
begin
  
end fuxzy;



entity idq is
  port (bawxxmxvl : buffer boolean; nsdsfy : in real);
end idq;



architecture lzsmibecit of idq is
  
begin
  
end lzsmibecit;



entity soi is
  port (lffdnril : inout real; nqrz : in time; a : linkage time; afgl : buffer bit);
end soi;



architecture wojyy of soi is
  signal bmhl : real;
  signal xtdyfy : boolean;
  signal qu : real;
  signal gipget : real;
  signal ycitrakg : boolean;
begin
  rsbsbx : entity work.idq
    port map (bawxxmxvl => ycitrakg, nsdsfy => lffdnril);
  xbqnuarwnm : entity work.dmnixm
    port map (m => gipget, q => lffdnril);
  ev : entity work.dmnixm
    port map (m => qu, q => lffdnril);
  szxqowja : entity work.idq
    port map (bawxxmxvl => xtdyfy, nsdsfy => bmhl);
end wojyy;



entity eut is
  port (rkay : out real; up : in real);
end eut;



architecture ib of eut is
  signal jtl : boolean;
  signal gpg : bit;
  signal pjhpwywewp : time;
  signal dllxmfvosg : time;
begin
  bfjurfecjg : entity work.soi
    port map (lffdnril => rkay, nqrz => dllxmfvosg, a => pjhpwywewp, afgl => gpg);
  zq : entity work.idq
    port map (bawxxmxvl => jtl, nsdsfy => rkay);
end ib;



-- Seed after: 15667453928344260519,8881251984038598933
