-- Seed: 18095452134473767269,14108379317686958507

library ieee;
use ieee.std_logic_1164.all;

entity kbhvf is
  port ( yqvwybrob : in boolean_vector(4 to 0)
  ; vagoufokq : out integer_vector(2 to 3)
  ; wvzrtdmsy : in std_logic_vector(2 to 4)
  ; wa : inout std_logic_vector(2 downto 4)
  );
end kbhvf;

architecture jgsa of kbhvf is
  
begin
  -- Single-driven assignments
  vagoufokq <= (120, 0);
  
  -- Multi-driven assignments
  wa <= wa;
end jgsa;

library ieee;
use ieee.std_logic_1164.all;

entity h is
  port (lpcxqx : linkage time; oyaiokmxfy : in std_logic_vector(0 downto 4); aujy : out boolean);
end h;

library ieee;
use ieee.std_logic_1164.all;

architecture zsjdsluy of h is
  signal eq : integer_vector(2 to 3);
  signal quvgchnde : std_logic_vector(2 downto 4);
  signal ejwqevzoop : integer_vector(2 to 3);
  signal lhndskb : std_logic_vector(2 downto 4);
  signal liqnr : std_logic_vector(2 to 4);
  signal boupdlg : integer_vector(2 to 3);
  signal ybceb : boolean_vector(4 to 0);
  signal vtjrduc : std_logic_vector(2 downto 4);
  signal n : std_logic_vector(2 to 4);
  signal vrgt : integer_vector(2 to 3);
  signal bwwdklm : boolean_vector(4 to 0);
begin
  sph : entity work.kbhvf
    port map (yqvwybrob => bwwdklm, vagoufokq => vrgt, wvzrtdmsy => n, wa => vtjrduc);
  vggpqhq : entity work.kbhvf
    port map (yqvwybrob => ybceb, vagoufokq => boupdlg, wvzrtdmsy => liqnr, wa => lhndskb);
  g : entity work.kbhvf
    port map (yqvwybrob => ybceb, vagoufokq => ejwqevzoop, wvzrtdmsy => n, wa => quvgchnde);
  nxu : entity work.kbhvf
    port map (yqvwybrob => bwwdklm, vagoufokq => eq, wvzrtdmsy => n, wa => vtjrduc);
  
  -- Multi-driven assignments
  vtjrduc <= (others => '0');
  n <= "L0W";
  n <= n;
  n <= liqnr;
end zsjdsluy;



-- Seed after: 16417152106450285128,14108379317686958507
