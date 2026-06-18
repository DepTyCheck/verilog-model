-- Seed: 6428357158322385763,8118127366649987907

entity xgtqsyz is
  port (oes : inout integer; mn : linkage time);
end xgtqsyz;

architecture imnsj of xgtqsyz is
  
begin
  -- Single-driven assignments
  oes <= 16#8D0D#;
end imnsj;

library ieee;
use ieee.std_logic_1164.all;

entity xphtp is
  port (waqv : buffer std_logic; ihex : out integer; lcslhz : linkage std_logic_vector(3 to 3); wet : out severity_level);
end xphtp;

architecture fzyyhkwgfo of xphtp is
  signal s : time;
  signal uihimjz : time;
  signal fppcaoi : integer;
  signal lh : time;
  signal irg : integer;
  signal qs : time;
  signal rjmabiklb : integer;
begin
  wttjx : entity work.xgtqsyz
    port map (oes => rjmabiklb, mn => qs);
  qtydyrv : entity work.xgtqsyz
    port map (oes => irg, mn => lh);
  dskdtp : entity work.xgtqsyz
    port map (oes => fppcaoi, mn => uihimjz);
  idtzcewy : entity work.xgtqsyz
    port map (oes => ihex, mn => s);
  
  -- Single-driven assignments
  wet <= WARNING;
  
  -- Multi-driven assignments
  waqv <= '0';
  waqv <= 'Z';
end fzyyhkwgfo;



-- Seed after: 2586058426606104340,8118127366649987907
