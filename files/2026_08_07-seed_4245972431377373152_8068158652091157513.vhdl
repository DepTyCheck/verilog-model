-- Seed: 4245972431377373152,8068158652091157513

library ieee;
use ieee.std_logic_1164.all;

entity vkexddmd is
  port (bsxalxngu : inout integer; zrf : linkage std_logic_vector(3 to 2); cpusfeg : linkage time; petjxo : out bit_vector(2 to 2));
end vkexddmd;

architecture ow of vkexddmd is
  
begin
  -- Single-driven assignments
  petjxo <= (others => '0');
  bsxalxngu <= 2#101#;
end ow;

entity i is
  port (vmc : linkage time; egza : buffer bit; pwueosfbw : buffer integer_vector(0 downto 4); ca : in real);
end i;

library ieee;
use ieee.std_logic_1164.all;

architecture niv of i is
  signal nznwngyy : bit_vector(2 to 2);
  signal cjqvzw : time;
  signal kvzijafam : integer;
  signal b : bit_vector(2 to 2);
  signal fj : std_logic_vector(3 to 2);
  signal wldpza : integer;
  signal mxfecoywb : bit_vector(2 to 2);
  signal tbefhixd : time;
  signal u : std_logic_vector(3 to 2);
  signal kothvzur : integer;
  signal ccstoez : bit_vector(2 to 2);
  signal zxrhpuxs : time;
  signal skkeehrix : std_logic_vector(3 to 2);
  signal psmsdnjfkd : integer;
begin
  ec : entity work.vkexddmd
    port map (bsxalxngu => psmsdnjfkd, zrf => skkeehrix, cpusfeg => zxrhpuxs, petjxo => ccstoez);
  inb : entity work.vkexddmd
    port map (bsxalxngu => kothvzur, zrf => u, cpusfeg => tbefhixd, petjxo => mxfecoywb);
  veqakuqryk : entity work.vkexddmd
    port map (bsxalxngu => wldpza, zrf => fj, cpusfeg => vmc, petjxo => b);
  mzv : entity work.vkexddmd
    port map (bsxalxngu => kvzijafam, zrf => skkeehrix, cpusfeg => cjqvzw, petjxo => nznwngyy);
  
  -- Single-driven assignments
  pwueosfbw <= pwueosfbw;
  egza <= '0';
  
  -- Multi-driven assignments
  fj <= "";
end niv;



-- Seed after: 3074222363124434128,8068158652091157513
