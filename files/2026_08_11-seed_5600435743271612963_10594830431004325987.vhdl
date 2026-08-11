-- Seed: 5600435743271612963,10594830431004325987

entity pzgr is
  port (m : inout integer; wtmdq : inout real_vector(1 downto 4));
end pzgr;

architecture z of pzgr is
  
begin
  -- Single-driven assignments
  m <= m;
  wtmdq <= wtmdq;
end z;

entity kijnlic is
  port (fsen : buffer real; vzbqyov : buffer time; vdemjsfww : buffer integer);
end kijnlic;

architecture pdyilw of kijnlic is
  signal clxepvg : real_vector(1 downto 4);
  signal bsddljpx : integer;
  signal cjxmyryted : real_vector(1 downto 4);
begin
  vqljrr : entity work.pzgr
    port map (m => vdemjsfww, wtmdq => cjxmyryted);
  ttkoe : entity work.pzgr
    port map (m => bsddljpx, wtmdq => clxepvg);
  
  -- Single-driven assignments
  vzbqyov <= 16#9_3_3_E# ns;
  fsen <= fsen;
end pdyilw;



-- Seed after: 16306850772783484717,10594830431004325987
