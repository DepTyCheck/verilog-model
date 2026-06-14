-- Seed: 15084613714135397333,6298042963991371649

library ieee;
use ieee.std_logic_1164.all;

entity asrlxtz is
  port (lccqykcdel : in std_logic; vvzttxf : linkage integer; lrisv : out std_logic_vector(1 downto 3));
end asrlxtz;



architecture iwyzom of asrlxtz is
  
begin
  
end iwyzom;

library ieee;
use ieee.std_logic_1164.all;

entity eazyngntz is
  port (ygvwfvv : inout bit_vector(2 to 2); nxvtgtxbf : inout std_logic; whclhu : out std_logic; rska : out real);
end eazyngntz;



architecture touulbw of eazyngntz is
  
begin
  
end touulbw;

library ieee;
use ieee.std_logic_1164.all;

entity miz is
  port (zqz : in std_logic; hrnpopi : inout std_logic_vector(1 downto 3); kbcdp : inout std_logic; c : out real_vector(2 to 2));
end miz;

library ieee;
use ieee.std_logic_1164.all;

architecture bojjceenv of miz is
  signal qyzfjtioj : std_logic_vector(1 downto 3);
  signal ql : integer;
  signal stcl : std_logic;
begin
  et : entity work.asrlxtz
    port map (lccqykcdel => stcl, vvzttxf => ql, lrisv => qyzfjtioj);
end bojjceenv;

library ieee;
use ieee.std_logic_1164.all;

entity nsxvnrj is
  port (euzxxh : out bit_vector(2 downto 2); ylkdi : in time; wdob : in std_logic_vector(0 to 0));
end nsxvnrj;

library ieee;
use ieee.std_logic_1164.all;

architecture r of nsxvnrj is
  signal qbpzi : std_logic_vector(1 downto 3);
  signal djgutgadqv : integer;
  signal eabnmf : std_logic;
  signal zlp : std_logic_vector(1 downto 3);
  signal bq : integer;
  signal ipiqhzl : integer;
  signal cxpjyedw : real_vector(2 to 2);
  signal rtplujlfa : std_logic;
  signal preu : std_logic_vector(1 downto 3);
  signal cwqdy : std_logic;
begin
  mfs : entity work.miz
    port map (zqz => cwqdy, hrnpopi => preu, kbcdp => rtplujlfa, c => cxpjyedw);
  tc : entity work.asrlxtz
    port map (lccqykcdel => rtplujlfa, vvzttxf => ipiqhzl, lrisv => preu);
  cgksbd : entity work.asrlxtz
    port map (lccqykcdel => rtplujlfa, vvzttxf => bq, lrisv => zlp);
  bfizukua : entity work.asrlxtz
    port map (lccqykcdel => eabnmf, vvzttxf => djgutgadqv, lrisv => qbpzi);
end r;



-- Seed after: 13629233190148351214,6298042963991371649
