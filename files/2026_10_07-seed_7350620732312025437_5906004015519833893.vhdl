-- Seed: 7350620732312025437,5906004015519833893

entity nfgwqpzvy is
  port (whnjcop : buffer time; rohkonovy : buffer integer_vector(2 downto 3));
end nfgwqpzvy;

architecture kfqzygrcdb of nfgwqpzvy is
  
begin
  -- Single-driven assignments
  rohkonovy <= (others => 0);
  whnjcop <= 2#01011.0_0# ps;
end kfqzygrcdb;

library ieee;
use ieee.std_logic_1164.all;

entity b is
  port (fyntjqxmug : in std_logic_vector(1 downto 0); blit : buffer severity_level);
end b;

architecture z of b is
  signal ahgqhbw : integer_vector(2 downto 3);
  signal yyspm : time;
  signal ofb : integer_vector(2 downto 3);
  signal kv : time;
  signal tgc : integer_vector(2 downto 3);
  signal coxj : time;
  signal tve : integer_vector(2 downto 3);
  signal zsz : time;
begin
  d : entity work.nfgwqpzvy
    port map (whnjcop => zsz, rohkonovy => tve);
  szuwe : entity work.nfgwqpzvy
    port map (whnjcop => coxj, rohkonovy => tgc);
  czlxcwag : entity work.nfgwqpzvy
    port map (whnjcop => kv, rohkonovy => ofb);
  snbbi : entity work.nfgwqpzvy
    port map (whnjcop => yyspm, rohkonovy => ahgqhbw);
  
  -- Single-driven assignments
  blit <= NOTE;
end z;

entity plaaf is
  port (kwk : inout boolean);
end plaaf;

library ieee;
use ieee.std_logic_1164.all;

architecture pcvudf of plaaf is
  signal gcmspbs : severity_level;
  signal pft : std_logic_vector(1 downto 0);
  signal t : severity_level;
  signal vzfu : std_logic_vector(1 downto 0);
  signal swokofxlk : severity_level;
  signal yyopktjfba : std_logic_vector(1 downto 0);
  signal s : integer_vector(2 downto 3);
  signal eosjz : time;
begin
  amutfas : entity work.nfgwqpzvy
    port map (whnjcop => eosjz, rohkonovy => s);
  au : entity work.b
    port map (fyntjqxmug => yyopktjfba, blit => swokofxlk);
  paphnsaxqc : entity work.b
    port map (fyntjqxmug => vzfu, blit => t);
  w : entity work.b
    port map (fyntjqxmug => pft, blit => gcmspbs);
  
  -- Single-driven assignments
  kwk <= FALSE;
  
  -- Multi-driven assignments
  yyopktjfba <= "0X";
  yyopktjfba <= yyopktjfba;
end pcvudf;



-- Seed after: 1935268036798571228,5906004015519833893
