-- Seed: 6433309274171125553,8891552411914730853

library ieee;
use ieee.std_logic_1164.all;

entity sdgd is
  port (uurrnmety : inout real; ebufbl : out real; i : inout std_logic_vector(0 downto 1); vjgmwhs : linkage integer);
end sdgd;

architecture pgjkkxwzni of sdgd is
  
begin
  -- Single-driven assignments
  ebufbl <= 2#1.0#;
  uurrnmety <= 14.2420;
  
  -- Multi-driven assignments
  i <= i;
  i <= i;
end pgjkkxwzni;

library ieee;
use ieee.std_logic_1164.all;

entity cnvnby is
  port (dz : buffer std_logic_vector(2 to 4); ugp : buffer std_logic_vector(0 to 2));
end cnvnby;

library ieee;
use ieee.std_logic_1164.all;

architecture fgdftkoaij of cnvnby is
  signal embaraaoz : integer;
  signal bgygsophwr : real;
  signal pnvn : real;
  signal y : integer;
  signal fmqgyjkscw : std_logic_vector(0 downto 1);
  signal lptffq : real;
  signal ykduscsh : real;
begin
  apbzgnw : entity work.sdgd
    port map (uurrnmety => ykduscsh, ebufbl => lptffq, i => fmqgyjkscw, vjgmwhs => y);
  ow : entity work.sdgd
    port map (uurrnmety => pnvn, ebufbl => bgygsophwr, i => fmqgyjkscw, vjgmwhs => embaraaoz);
end fgdftkoaij;

entity ky is
  port (gtxrw : buffer time_vector(3 to 0); fab : buffer time);
end ky;

library ieee;
use ieee.std_logic_1164.all;

architecture bevmk of ky is
  signal ikhdwt : integer;
  signal xjswvnscqe : std_logic_vector(0 downto 1);
  signal torwaqsc : real;
  signal r : real;
  signal hvm : integer;
  signal nt : std_logic_vector(0 downto 1);
  signal qjxvfyrw : real;
  signal xezphs : real;
  signal upyd : std_logic_vector(0 to 2);
  signal hjd : std_logic_vector(2 to 4);
begin
  fvjw : entity work.cnvnby
    port map (dz => hjd, ugp => upyd);
  yadrdikpe : entity work.sdgd
    port map (uurrnmety => xezphs, ebufbl => qjxvfyrw, i => nt, vjgmwhs => hvm);
  mvzplbje : entity work.sdgd
    port map (uurrnmety => r, ebufbl => torwaqsc, i => xjswvnscqe, vjgmwhs => ikhdwt);
  
  -- Single-driven assignments
  fab <= 2024.2 fs;
  gtxrw <= (others => 0 ns);
end bevmk;



-- Seed after: 4171011727175051452,8891552411914730853
