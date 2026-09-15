-- Seed: 9308549083896869079,13613332369802491303

library ieee;
use ieee.std_logic_1164.all;

entity nmobbxwhp is
  port (njgaau : buffer std_logic_vector(2 downto 0); uqpygpaj : in time; trbmuhykoy : buffer std_logic_vector(3 to 1));
end nmobbxwhp;

architecture cjdfihfaog of nmobbxwhp is
  
begin
  -- Multi-driven assignments
  trbmuhykoy <= trbmuhykoy;
  trbmuhykoy <= (others => '0');
end cjdfihfaog;

entity dphjgekdp is
  port (zf : linkage bit_vector(3 to 3));
end dphjgekdp;

library ieee;
use ieee.std_logic_1164.all;

architecture wpwkcao of dphjgekdp is
  signal g : std_logic_vector(3 to 1);
  signal frdju : time;
  signal owzsxmxryk : std_logic_vector(2 downto 0);
begin
  fzhblyhzft : entity work.nmobbxwhp
    port map (njgaau => owzsxmxryk, uqpygpaj => frdju, trbmuhykoy => g);
end wpwkcao;

library ieee;
use ieee.std_logic_1164.all;

entity qlp is
  port (d : inout std_logic_vector(4 to 0); ddibnh : buffer real);
end qlp;

architecture f of qlp is
  signal eug : bit_vector(3 to 3);
begin
  ct : entity work.dphjgekdp
    port map (zf => eug);
  
  -- Single-driven assignments
  ddibnh <= 0_0.2_1;
end f;

entity pc is
  port (emicq : in time);
end pc;

architecture feuluj of pc is
  
begin
  
end feuluj;



-- Seed after: 17779501579958263319,13613332369802491303
