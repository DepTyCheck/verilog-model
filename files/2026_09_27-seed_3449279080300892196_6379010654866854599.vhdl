-- Seed: 3449279080300892196,6379010654866854599

library ieee;
use ieee.std_logic_1164.all;

entity rgzmlb is
  port (aqixfdqbk : inout std_logic_vector(3 downto 4); kb : buffer integer_vector(1 to 0));
end rgzmlb;

architecture dy of rgzmlb is
  
begin
  -- Single-driven assignments
  kb <= kb;
  
  -- Multi-driven assignments
  aqixfdqbk <= aqixfdqbk;
  aqixfdqbk <= "";
end dy;

library ieee;
use ieee.std_logic_1164.all;

entity ahdsnrzoyf is
  port (mkkqrhbrkb : in std_logic_vector(2 downto 2); umgzoqpt : in std_logic_vector(0 downto 1));
end ahdsnrzoyf;

library ieee;
use ieee.std_logic_1164.all;

architecture twp of ahdsnrzoyf is
  signal s : integer_vector(1 to 0);
  signal fxsadzo : integer_vector(1 to 0);
  signal gviwgom : integer_vector(1 to 0);
  signal i : std_logic_vector(3 downto 4);
begin
  legmbxvfys : entity work.rgzmlb
    port map (aqixfdqbk => i, kb => gviwgom);
  kflzxlih : entity work.rgzmlb
    port map (aqixfdqbk => i, kb => fxsadzo);
  ozr : entity work.rgzmlb
    port map (aqixfdqbk => i, kb => s);
end twp;

library ieee;
use ieee.std_logic_1164.all;

entity laotjwgdt is
  port (luodemo : out real; tqsalsnmz : out std_logic);
end laotjwgdt;

library ieee;
use ieee.std_logic_1164.all;

architecture dv of laotjwgdt is
  signal eypbnzkv : integer_vector(1 to 0);
  signal stfigtly : integer_vector(1 to 0);
  signal eurwbook : std_logic_vector(3 downto 4);
  signal wjn : std_logic_vector(3 downto 4);
  signal fmnacsqbb : std_logic_vector(2 downto 2);
begin
  ktdtd : entity work.ahdsnrzoyf
    port map (mkkqrhbrkb => fmnacsqbb, umgzoqpt => wjn);
  ehmgs : entity work.ahdsnrzoyf
    port map (mkkqrhbrkb => fmnacsqbb, umgzoqpt => wjn);
  vyxw : entity work.rgzmlb
    port map (aqixfdqbk => eurwbook, kb => stfigtly);
  qrnrckg : entity work.rgzmlb
    port map (aqixfdqbk => wjn, kb => eypbnzkv);
  
  -- Multi-driven assignments
  tqsalsnmz <= tqsalsnmz;
  fmnacsqbb <= (others => 'U');
end dv;



-- Seed after: 3322553544130407432,6379010654866854599
