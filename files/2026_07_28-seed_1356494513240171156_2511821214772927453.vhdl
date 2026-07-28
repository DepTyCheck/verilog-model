-- Seed: 1356494513240171156,2511821214772927453

library ieee;
use ieee.std_logic_1164.all;

entity zt is
  port (r : inout time_vector(1 downto 2); kforrvj : inout std_logic_vector(2 downto 1); clkjassded : buffer std_logic);
end zt;

architecture vtgm of zt is
  
begin
  -- Single-driven assignments
  r <= (others => 0 ns);
  
  -- Multi-driven assignments
  clkjassded <= 'Z';
  clkjassded <= 'Z';
  clkjassded <= clkjassded;
end vtgm;

entity aallf is
  port (ie : out real; vunz : buffer real);
end aallf;

library ieee;
use ieee.std_logic_1164.all;

architecture oiauqli of aallf is
  signal roxlr : time_vector(1 downto 2);
  signal u : time_vector(1 downto 2);
  signal nvlkkblndw : std_logic;
  signal ob : std_logic_vector(2 downto 1);
  signal ptlqexbvgo : time_vector(1 downto 2);
begin
  ze : entity work.zt
    port map (r => ptlqexbvgo, kforrvj => ob, clkjassded => nvlkkblndw);
  yqebt : entity work.zt
    port map (r => u, kforrvj => ob, clkjassded => nvlkkblndw);
  qjo : entity work.zt
    port map (r => roxlr, kforrvj => ob, clkjassded => nvlkkblndw);
  
  -- Multi-driven assignments
  nvlkkblndw <= 'H';
end oiauqli;

entity irgmkqkhtv is
  port (aqp : in integer);
end irgmkqkhtv;

library ieee;
use ieee.std_logic_1164.all;

architecture j of irgmkqkhtv is
  signal bvcjl : time_vector(1 downto 2);
  signal ayqp : time_vector(1 downto 2);
  signal svszd : std_logic;
  signal bkpleznhqr : std_logic_vector(2 downto 1);
  signal wnuvmbwa : time_vector(1 downto 2);
begin
  hvpuof : entity work.zt
    port map (r => wnuvmbwa, kforrvj => bkpleznhqr, clkjassded => svszd);
  rmvjhfutso : entity work.zt
    port map (r => ayqp, kforrvj => bkpleznhqr, clkjassded => svszd);
  wr : entity work.zt
    port map (r => bvcjl, kforrvj => bkpleznhqr, clkjassded => svszd);
  
  -- Multi-driven assignments
  bkpleznhqr <= "-H";
  svszd <= svszd;
  svszd <= 'U';
  svszd <= svszd;
end j;



-- Seed after: 5934514766093519637,2511821214772927453
