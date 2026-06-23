-- Seed: 7976248785940768955,8421704836678237495

library ieee;
use ieee.std_logic_1164.all;

entity cz is
  port (rwyddx : out std_logic; v : in boolean; fktwzdat : buffer integer);
end cz;

architecture vdprxdjw of cz is
  
begin
  -- Single-driven assignments
  fktwzdat <= 16#2_D_B_E_8#;
  
  -- Multi-driven assignments
  rwyddx <= '0';
  rwyddx <= 'H';
  rwyddx <= 'X';
end vdprxdjw;

entity iyfugczxf is
  port ( ytoubi : inout real
  ; jbzpaatqsa : buffer integer_vector(0 downto 1)
  ; jbuz : linkage time_vector(1 to 2)
  ; sds : inout boolean_vector(1 downto 0)
  );
end iyfugczxf;

library ieee;
use ieee.std_logic_1164.all;

architecture hexbl of iyfugczxf is
  signal bekurh : integer;
  signal mtkosvsf : boolean;
  signal c : integer;
  signal dwwmhcvua : boolean;
  signal dwknzeyx : integer;
  signal lhkii : boolean;
  signal k : std_logic;
  signal haxppwgi : integer;
  signal fxfqa : boolean;
  signal vtokb : std_logic;
begin
  gi : entity work.cz
    port map (rwyddx => vtokb, v => fxfqa, fktwzdat => haxppwgi);
  ztx : entity work.cz
    port map (rwyddx => k, v => lhkii, fktwzdat => dwknzeyx);
  rcmtvqim : entity work.cz
    port map (rwyddx => vtokb, v => dwwmhcvua, fktwzdat => c);
  i : entity work.cz
    port map (rwyddx => vtokb, v => mtkosvsf, fktwzdat => bekurh);
  
  -- Single-driven assignments
  dwwmhcvua <= FALSE;
  mtkosvsf <= FALSE;
  lhkii <= FALSE;
end hexbl;



-- Seed after: 16304437467363930535,8421704836678237495
