-- Seed: 12336004937513237912,17924494779688682807

library ieee;
use ieee.std_logic_1164.all;

entity gpugs is
  port (byeinlr : linkage std_logic_vector(0 downto 0); mycf : inout real; qdjw : linkage integer; aouh : buffer boolean_vector(2 downto 1));
end gpugs;

architecture ygfflbn of gpugs is
  
begin
  -- Single-driven assignments
  aouh <= (FALSE, TRUE);
end ygfflbn;

library ieee;
use ieee.std_logic_1164.all;

entity gap is
  port (kfltnpbvg : in std_logic);
end gap;

library ieee;
use ieee.std_logic_1164.all;

architecture kvnpo of gap is
  signal ktfcemydk : boolean_vector(2 downto 1);
  signal fbptpxapf : integer;
  signal sipsvu : real;
  signal csp : std_logic_vector(0 downto 0);
  signal hnkffks : boolean_vector(2 downto 1);
  signal fuptadee : integer;
  signal bbrcmwh : real;
  signal swyc : boolean_vector(2 downto 1);
  signal ujeqs : integer;
  signal sjkdy : real;
  signal dtwdrbsif : std_logic_vector(0 downto 0);
  signal j : boolean_vector(2 downto 1);
  signal igmlihwt : integer;
  signal zzougns : real;
  signal ygy : std_logic_vector(0 downto 0);
begin
  cce : entity work.gpugs
    port map (byeinlr => ygy, mycf => zzougns, qdjw => igmlihwt, aouh => j);
  tgsdnttsd : entity work.gpugs
    port map (byeinlr => dtwdrbsif, mycf => sjkdy, qdjw => ujeqs, aouh => swyc);
  ktozxclm : entity work.gpugs
    port map (byeinlr => dtwdrbsif, mycf => bbrcmwh, qdjw => fuptadee, aouh => hnkffks);
  urtlfs : entity work.gpugs
    port map (byeinlr => csp, mycf => sipsvu, qdjw => fbptpxapf, aouh => ktfcemydk);
  
  -- Multi-driven assignments
  ygy <= "H";
  ygy <= "H";
  dtwdrbsif <= (others => 'W');
end kvnpo;

library ieee;
use ieee.std_logic_1164.all;

entity chryddtuyf is
  port (zdwsiyet : linkage string(3 downto 2); lythr : linkage std_logic_vector(2 to 3); kejv : buffer integer; pgisrlhm : linkage std_logic);
end chryddtuyf;

library ieee;
use ieee.std_logic_1164.all;

architecture kav of chryddtuyf is
  signal xrwpm : boolean_vector(2 downto 1);
  signal koebkyn : real;
  signal yief : boolean_vector(2 downto 1);
  signal zpreync : integer;
  signal xjhdg : real;
  signal qqstclimr : std_logic_vector(0 downto 0);
  signal gwbmlvy : std_logic;
  signal mptt : std_logic;
begin
  ygowolbz : entity work.gap
    port map (kfltnpbvg => mptt);
  zmzg : entity work.gap
    port map (kfltnpbvg => gwbmlvy);
  uupkgkkrq : entity work.gpugs
    port map (byeinlr => qqstclimr, mycf => xjhdg, qdjw => zpreync, aouh => yief);
  dep : entity work.gpugs
    port map (byeinlr => qqstclimr, mycf => koebkyn, qdjw => kejv, aouh => xrwpm);
  
  -- Multi-driven assignments
  mptt <= '0';
  mptt <= 'X';
  gwbmlvy <= '-';
  mptt <= '0';
end kav;



-- Seed after: 1802208936565527713,17924494779688682807
