-- Seed: 6384227242845752493,7198033922882419595

entity nxjnvw is
  port (b : buffer integer);
end nxjnvw;

architecture o of nxjnvw is
  
begin
  -- Single-driven assignments
  b <= b;
end o;

entity tio is
  port (hqhssceyg : out real);
end tio;

architecture vgyp of tio is
  
begin
  -- Single-driven assignments
  hqhssceyg <= 0_3_1_2.1230;
end vgyp;

entity h is
  port (zmtozn : inout integer; lo : out time);
end h;

architecture xeu of h is
  
begin
  pkvycj : entity work.nxjnvw
    port map (b => zmtozn);
end xeu;



-- Seed after: 12015612182469259697,7198033922882419595
