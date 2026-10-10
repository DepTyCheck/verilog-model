-- Seed: 13469085005284974499,511364357853360275

library ieee;
use ieee.std_logic_1164.all;

entity niirwft is
  port (sleblfx : buffer std_logic; wagc : inout integer; smhz : buffer std_logic; ihwydjpz : out std_logic_vector(3 to 1));
end niirwft;

architecture prlotcw of niirwft is
  
begin
  -- Single-driven assignments
  wagc <= 2_2_0;
  
  -- Multi-driven assignments
  ihwydjpz <= ihwydjpz;
end prlotcw;

library ieee;
use ieee.std_logic_1164.all;

entity lwm is
  port (snjcyahbk : buffer time; eome : buffer std_logic; qzag : in character; apzclztqpe : inout time_vector(1 downto 2));
end lwm;

library ieee;
use ieee.std_logic_1164.all;

architecture suiyndqpj of lwm is
  signal beiqggwi : std_logic;
  signal xg : integer;
  signal xt : std_logic;
  signal ptvogpqy : integer;
  signal hjofhbp : std_logic_vector(3 to 1);
  signal knpueez : integer;
  signal ggwpnt : std_logic;
  signal ljf : std_logic_vector(3 to 1);
  signal en : integer;
begin
  ogxk : entity work.niirwft
    port map (sleblfx => eome, wagc => en, smhz => eome, ihwydjpz => ljf);
  eshdcslutf : entity work.niirwft
    port map (sleblfx => ggwpnt, wagc => knpueez, smhz => eome, ihwydjpz => hjofhbp);
  iyczmmam : entity work.niirwft
    port map (sleblfx => eome, wagc => ptvogpqy, smhz => ggwpnt, ihwydjpz => hjofhbp);
  mvj : entity work.niirwft
    port map (sleblfx => xt, wagc => xg, smhz => beiqggwi, ihwydjpz => ljf);
  
  -- Single-driven assignments
  apzclztqpe <= (others => 0 ns);
  snjcyahbk <= snjcyahbk;
end suiyndqpj;



-- Seed after: 11862877712849233834,511364357853360275
