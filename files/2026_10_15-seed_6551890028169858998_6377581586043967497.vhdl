-- Seed: 6551890028169858998,6377581586043967497

entity fwabzpstbl is
  port (fdqemgslm : buffer time; yxx : buffer time);
end fwabzpstbl;

architecture hnafb of fwabzpstbl is
  
begin
  -- Single-driven assignments
  yxx <= fdqemgslm;
  fdqemgslm <= yxx;
end hnafb;

entity j is
  port (pxqbhbhw : inout bit; ogesynx : inout time; jjkuuzqw : in real);
end j;

architecture qurysube of j is
  signal qfxrrfv : time;
begin
  yc : entity work.fwabzpstbl
    port map (fdqemgslm => qfxrrfv, yxx => ogesynx);
  
  -- Single-driven assignments
  pxqbhbhw <= '1';
end qurysube;



-- Seed after: 834716027017083994,6377581586043967497
