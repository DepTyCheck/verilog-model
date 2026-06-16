-- Seed: 12584425856419966660,5472058987609252853

library ieee;
use ieee.std_logic_1164.all;

entity lp is
  port (tyzfcspd : buffer std_logic; zjoqzrnfji : linkage bit_vector(2 to 1); tjrtenw : in bit_vector(3 downto 2));
end lp;

architecture gxnu of lp is
  
begin
  -- Multi-driven assignments
  tyzfcspd <= 'L';
  tyzfcspd <= 'Z';
end gxnu;

library ieee;
use ieee.std_logic_1164.all;

entity vabsxwfw is
  port (pwzsu : linkage boolean_vector(3 to 4); ciyqqnmw : in std_logic_vector(0 to 3); jtnvsneb : out real);
end vabsxwfw;

library ieee;
use ieee.std_logic_1164.all;

architecture calt of vabsxwfw is
  signal wcj : bit_vector(2 to 1);
  signal rmaphb : bit_vector(2 to 1);
  signal hgazvarmu : bit_vector(3 downto 2);
  signal ensjc : bit_vector(2 to 1);
  signal iejtkph : std_logic;
begin
  iahdxlbvl : entity work.lp
    port map (tyzfcspd => iejtkph, zjoqzrnfji => ensjc, tjrtenw => hgazvarmu);
  xafq : entity work.lp
    port map (tyzfcspd => iejtkph, zjoqzrnfji => rmaphb, tjrtenw => hgazvarmu);
  tn : entity work.lp
    port map (tyzfcspd => iejtkph, zjoqzrnfji => wcj, tjrtenw => hgazvarmu);
end calt;

entity nesq is
  port (ig : in integer_vector(3 to 2); egrfv : out severity_level; qwzgsvqb : linkage integer_vector(1 downto 0));
end nesq;

library ieee;
use ieee.std_logic_1164.all;

architecture rjrqiw of nesq is
  signal dugfjonxpe : bit_vector(3 downto 2);
  signal dehxzepy : bit_vector(2 to 1);
  signal xkdfccxjlj : std_logic;
  signal ivejajb : real;
  signal ijmm : std_logic_vector(0 to 3);
  signal g : boolean_vector(3 to 4);
begin
  iaapsy : entity work.vabsxwfw
    port map (pwzsu => g, ciyqqnmw => ijmm, jtnvsneb => ivejajb);
  x : entity work.lp
    port map (tyzfcspd => xkdfccxjlj, zjoqzrnfji => dehxzepy, tjrtenw => dugfjonxpe);
  
  -- Single-driven assignments
  egrfv <= WARNING;
  dugfjonxpe <= ('1', '1');
  
  -- Multi-driven assignments
  ijmm <= "LL01";
  ijmm <= "01-L";
  ijmm <= "-Z-0";
  xkdfccxjlj <= '-';
end rjrqiw;



-- Seed after: 2286896875226622834,5472058987609252853
