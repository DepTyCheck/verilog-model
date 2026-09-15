-- Seed: 14024407373864069619,13613332369802491303

library ieee;
use ieee.std_logic_1164.all;

entity auflovckuq is
  port (vth : out std_logic_vector(1 downto 1); qfcutqcay : buffer std_logic_vector(4 downto 2); fho : out integer);
end auflovckuq;

architecture qelqdlxry of auflovckuq is
  
begin
  -- Single-driven assignments
  fho <= fho;
end qelqdlxry;

library ieee;
use ieee.std_logic_1164.all;

entity dcxhisina is
  port (vxpmtbdv : inout std_logic_vector(2 to 1); zwaxdw : out real; gx : buffer bit_vector(4 to 3); adevnvj : in time);
end dcxhisina;

library ieee;
use ieee.std_logic_1164.all;

architecture fmhtgblfge of dcxhisina is
  signal oh : integer;
  signal dbcl : std_logic_vector(4 downto 2);
  signal jxqeqtamkt : integer;
  signal vknbsntcug : std_logic_vector(1 downto 1);
  signal wrojvwcmhj : integer;
  signal xrsr : std_logic_vector(4 downto 2);
  signal vvowtjrkv : std_logic_vector(1 downto 1);
begin
  hdfufose : entity work.auflovckuq
    port map (vth => vvowtjrkv, qfcutqcay => xrsr, fho => wrojvwcmhj);
  dmfvvtmh : entity work.auflovckuq
    port map (vth => vknbsntcug, qfcutqcay => xrsr, fho => jxqeqtamkt);
  gckdpjc : entity work.auflovckuq
    port map (vth => vvowtjrkv, qfcutqcay => dbcl, fho => oh);
  
  -- Multi-driven assignments
  dbcl <= ('0', 'H', '1');
  vknbsntcug <= (others => 'U');
end fmhtgblfge;

entity r is
  port (frjnzkb : inout real; scnchi : in bit_vector(4 downto 3));
end r;

library ieee;
use ieee.std_logic_1164.all;

architecture vqozv of r is
  signal lpiwfv : time;
  signal hcvpmhqm : bit_vector(4 to 3);
  signal brjjzo : real;
  signal kwijtdnau : std_logic_vector(2 to 1);
begin
  pqqeaxbm : entity work.dcxhisina
    port map (vxpmtbdv => kwijtdnau, zwaxdw => brjjzo, gx => hcvpmhqm, adevnvj => lpiwfv);
  
  -- Multi-driven assignments
  kwijtdnau <= "";
  kwijtdnau <= kwijtdnau;
  kwijtdnau <= kwijtdnau;
  kwijtdnau <= "";
end vqozv;



-- Seed after: 11640995132350544975,13613332369802491303
