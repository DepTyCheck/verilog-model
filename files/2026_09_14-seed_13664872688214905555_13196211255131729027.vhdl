-- Seed: 13664872688214905555,13196211255131729027

library ieee;
use ieee.std_logic_1164.all;

entity iymnhzy is
  port (p : out real; fjyaow : inout std_logic; bi : out integer; dq : in std_logic_vector(1 downto 2));
end iymnhzy;

architecture tdiosb of iymnhzy is
  
begin
  -- Single-driven assignments
  bi <= 1;
  p <= 8#7.0_2#;
end tdiosb;

library ieee;
use ieee.std_logic_1164.all;

entity ruaan is
  port (ftfh : buffer bit; khq : out bit_vector(4 to 1); hztdqainb : out std_logic; pvbuzdvms : buffer std_logic);
end ruaan;

library ieee;
use ieee.std_logic_1164.all;

architecture jrqzcd of ruaan is
  signal jfjhzhr : std_logic_vector(1 downto 2);
  signal khblujoki : integer;
  signal hjs : std_logic;
  signal vmtdmqd : real;
  signal vczdvdf : std_logic_vector(1 downto 2);
  signal cxw : integer;
  signal ydu : real;
begin
  a : entity work.iymnhzy
    port map (p => ydu, fjyaow => pvbuzdvms, bi => cxw, dq => vczdvdf);
  tnvk : entity work.iymnhzy
    port map (p => vmtdmqd, fjyaow => hjs, bi => khblujoki, dq => jfjhzhr);
  
  -- Single-driven assignments
  khq <= khq;
  ftfh <= '0';
  
  -- Multi-driven assignments
  hjs <= 'X';
  pvbuzdvms <= 'H';
end jrqzcd;



-- Seed after: 8360887926509805273,13196211255131729027
