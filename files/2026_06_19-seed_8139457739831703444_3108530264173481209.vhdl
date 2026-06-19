-- Seed: 8139457739831703444,3108530264173481209

library ieee;
use ieee.std_logic_1164.all;

entity cmgkwszc is
  port (ttq : inout time_vector(1 to 2); avcc : buffer std_logic; ystqiovppe : linkage time);
end cmgkwszc;

architecture uonaaoejzf of cmgkwszc is
  
begin
  -- Single-driven assignments
  ttq <= (3 ns, 4 sec);
end uonaaoejzf;

entity h is
  port (zkyvqbsw : buffer real_vector(0 downto 2));
end h;

library ieee;
use ieee.std_logic_1164.all;

architecture kjwayjbox of h is
  signal ieprtuslw : time;
  signal ytici : std_logic;
  signal anyytg : time_vector(1 to 2);
  signal aupdqqswh : time;
  signal zayvmaenf : time_vector(1 to 2);
  signal yvemmy : time;
  signal vl : std_logic;
  signal bqyffubvoi : time_vector(1 to 2);
  signal uer : time;
  signal gcw : std_logic;
  signal su : time_vector(1 to 2);
begin
  bm : entity work.cmgkwszc
    port map (ttq => su, avcc => gcw, ystqiovppe => uer);
  yd : entity work.cmgkwszc
    port map (ttq => bqyffubvoi, avcc => vl, ystqiovppe => yvemmy);
  zkaflx : entity work.cmgkwszc
    port map (ttq => zayvmaenf, avcc => gcw, ystqiovppe => aupdqqswh);
  ysxiruc : entity work.cmgkwszc
    port map (ttq => anyytg, avcc => ytici, ystqiovppe => ieprtuslw);
  
  -- Single-driven assignments
  zkyvqbsw <= (others => 0.0);
  
  -- Multi-driven assignments
  gcw <= 'H';
end kjwayjbox;



-- Seed after: 9960721796188563881,3108530264173481209
