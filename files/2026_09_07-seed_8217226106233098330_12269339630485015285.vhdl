-- Seed: 8217226106233098330,12269339630485015285

library ieee;
use ieee.std_logic_1164.all;

entity yv is
  port (tjcusin : linkage integer_vector(1 downto 4); iudopaq : inout real; yikxccgq : linkage std_logic_vector(3 downto 3));
end yv;

architecture ekpuejali of yv is
  
begin
  -- Single-driven assignments
  iudopaq <= 000.4_4_3_3_1;
end ekpuejali;

entity lvkpa is
  port (yxaqchqdmu : buffer time);
end lvkpa;

architecture moizwawi of lvkpa is
  
begin
  -- Single-driven assignments
  yxaqchqdmu <= 4.0_0_2_3_1 fs;
end moizwawi;

library ieee;
use ieee.std_logic_1164.all;

entity n is
  port (zjn : linkage real; vlgrwyum : linkage std_logic);
end n;

library ieee;
use ieee.std_logic_1164.all;

architecture fkt of n is
  signal wkoxvtj : time;
  signal yk : time;
  signal qrrjvv : std_logic_vector(3 downto 3);
  signal dbrcgll : real;
  signal rgqy : integer_vector(1 downto 4);
  signal g : time;
begin
  zqvucsqkp : entity work.lvkpa
    port map (yxaqchqdmu => g);
  sraqrdaa : entity work.yv
    port map (tjcusin => rgqy, iudopaq => dbrcgll, yikxccgq => qrrjvv);
  z : entity work.lvkpa
    port map (yxaqchqdmu => yk);
  mteluzx : entity work.lvkpa
    port map (yxaqchqdmu => wkoxvtj);
  
  -- Multi-driven assignments
  qrrjvv <= "-";
  qrrjvv <= "H";
end fkt;



-- Seed after: 1587803691026167815,12269339630485015285
