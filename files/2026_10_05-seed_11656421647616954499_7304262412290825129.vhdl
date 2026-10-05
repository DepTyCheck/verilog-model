-- Seed: 11656421647616954499,7304262412290825129

library ieee;
use ieee.std_logic_1164.all;

entity ttlmxevc is
  port ( tjvmw : linkage integer_vector(2 downto 4)
  ; alywxaf : inout real_vector(0 downto 3)
  ; debfe : in std_logic
  ; slrtrh : in std_logic_vector(3 to 2)
  );
end ttlmxevc;

architecture qrjl of ttlmxevc is
  
begin
  -- Single-driven assignments
  alywxaf <= (others => 0.0);
end qrjl;

entity a is
  port (qnjvlun : buffer boolean; njbe : linkage integer);
end a;

library ieee;
use ieee.std_logic_1164.all;

architecture gart of a is
  signal rtaxeuo : std_logic;
  signal hppejp : real_vector(0 downto 3);
  signal hi : integer_vector(2 downto 4);
  signal uxipyynvni : std_logic_vector(3 to 2);
  signal irprftyy : real_vector(0 downto 3);
  signal zpg : integer_vector(2 downto 4);
  signal dtvccirl : std_logic_vector(3 to 2);
  signal pfgtckxetw : std_logic;
  signal x : real_vector(0 downto 3);
  signal cl : integer_vector(2 downto 4);
begin
  wxlmthenhn : entity work.ttlmxevc
    port map (tjvmw => cl, alywxaf => x, debfe => pfgtckxetw, slrtrh => dtvccirl);
  qrjxbsuzcu : entity work.ttlmxevc
    port map (tjvmw => zpg, alywxaf => irprftyy, debfe => pfgtckxetw, slrtrh => uxipyynvni);
  pnwv : entity work.ttlmxevc
    port map (tjvmw => hi, alywxaf => hppejp, debfe => rtaxeuo, slrtrh => dtvccirl);
  
  -- Single-driven assignments
  qnjvlun <= qnjvlun;
end gart;



-- Seed after: 2136279332312102959,7304262412290825129
