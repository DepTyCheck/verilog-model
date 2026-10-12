-- Seed: 1733230774478308928,3426943171337919827

library ieee;
use ieee.std_logic_1164.all;

entity jgwany is
  port (zynyszallv : inout std_logic_vector(4 to 0); ygakanoh : inout boolean_vector(4 downto 1); lxnibme : buffer std_logic_vector(2 downto 2));
end jgwany;

architecture vdgdzdmei of jgwany is
  
begin
  -- Single-driven assignments
  ygakanoh <= (FALSE, TRUE, FALSE, FALSE);
  
  -- Multi-driven assignments
  lxnibme <= (others => 'X');
  zynyszallv <= (others => '0');
  lxnibme <= lxnibme;
end vdgdzdmei;

library ieee;
use ieee.std_logic_1164.all;

entity jlyf is
  port (bzjn : linkage std_logic);
end jlyf;

library ieee;
use ieee.std_logic_1164.all;

architecture zocdbwu of jlyf is
  signal fm : std_logic_vector(2 downto 2);
  signal abnt : boolean_vector(4 downto 1);
  signal riza : std_logic_vector(4 to 0);
begin
  kdfmcqtei : entity work.jgwany
    port map (zynyszallv => riza, ygakanoh => abnt, lxnibme => fm);
  
  -- Multi-driven assignments
  fm <= "W";
end zocdbwu;

entity wcmu is
  port (vjpbgw : buffer severity_level; aefvyyqxi : out time; rwpv : in time; hgayakucy : buffer real);
end wcmu;

library ieee;
use ieee.std_logic_1164.all;

architecture oqcdkufmy of wcmu is
  signal brnqhu : std_logic;
begin
  tbuwjqtfh : entity work.jlyf
    port map (bzjn => brnqhu);
  
  -- Single-driven assignments
  aefvyyqxi <= 16#542B.3C# ps;
end oqcdkufmy;



-- Seed after: 1186060032211511924,3426943171337919827
