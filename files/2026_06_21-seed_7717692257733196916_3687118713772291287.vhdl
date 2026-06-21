-- Seed: 7717692257733196916,3687118713772291287

library ieee;
use ieee.std_logic_1164.all;

entity mlfn is
  port (mk : in integer; v : buffer std_logic_vector(0 downto 1); irzogmv : in time; duvysvlng : out boolean_vector(1 downto 1));
end mlfn;

architecture twigkg of mlfn is
  
begin
  -- Single-driven assignments
  duvysvlng <= (others => TRUE);
  
  -- Multi-driven assignments
  v <= (others => '0');
end twigkg;

entity fivnjtqr is
  port (wom : out severity_level; unvqlvskc : out time; yqp : out time);
end fivnjtqr;

library ieee;
use ieee.std_logic_1164.all;

architecture ygzmd of fivnjtqr is
  signal pq : boolean_vector(1 downto 1);
  signal lqcway : time;
  signal zlzpr : std_logic_vector(0 downto 1);
  signal ghyf : integer;
  signal z : boolean_vector(1 downto 1);
  signal dhorfxx : std_logic_vector(0 downto 1);
  signal clyz : boolean_vector(1 downto 1);
  signal klwyerdqgi : std_logic_vector(0 downto 1);
  signal rlfajfpl : integer;
begin
  wuexwi : entity work.mlfn
    port map (mk => rlfajfpl, v => klwyerdqgi, irzogmv => yqp, duvysvlng => clyz);
  dsqv : entity work.mlfn
    port map (mk => rlfajfpl, v => dhorfxx, irzogmv => yqp, duvysvlng => z);
  zsjdmjcrk : entity work.mlfn
    port map (mk => ghyf, v => zlzpr, irzogmv => lqcway, duvysvlng => pq);
  
  -- Single-driven assignments
  ghyf <= 342;
  rlfajfpl <= 1_0_3;
end ygzmd;

entity aqzrxnqc is
  port (fsbrft : in integer);
end aqzrxnqc;

library ieee;
use ieee.std_logic_1164.all;

architecture r of aqzrxnqc is
  signal kahbjue : boolean_vector(1 downto 1);
  signal fwgkvxgm : time;
  signal g : std_logic_vector(0 downto 1);
  signal buuljp : integer;
begin
  wfu : entity work.mlfn
    port map (mk => buuljp, v => g, irzogmv => fwgkvxgm, duvysvlng => kahbjue);
  
  -- Multi-driven assignments
  g <= (others => '0');
  g <= (others => '0');
  g <= (others => '0');
  g <= (others => '0');
end r;



-- Seed after: 10293624873662546654,3687118713772291287
