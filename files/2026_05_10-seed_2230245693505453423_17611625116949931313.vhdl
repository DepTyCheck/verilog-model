-- Seed: 2230245693505453423,17611625116949931313

library ieee;
use ieee.std_logic_1164.all;

entity mtnc is
  port (awuiplcik : linkage std_logic; jpk : inout time; d : linkage real);
end mtnc;



architecture p of mtnc is
  
begin
  
end p;

library ieee;
use ieee.std_logic_1164.all;

entity arcn is
  port (kd : in std_logic);
end arcn;

library ieee;
use ieee.std_logic_1164.all;

architecture cctsbsvtn of arcn is
  signal tfssztzlgb : time;
  signal zw : std_logic;
  signal dvfrjtuo : time;
  signal utonrswii : real;
  signal etrrd : time;
  signal z : std_logic;
begin
  k : entity work.mtnc
    port map (awuiplcik => z, jpk => etrrd, d => utonrswii);
  ywzd : entity work.mtnc
    port map (awuiplcik => kd, jpk => dvfrjtuo, d => utonrswii);
  hu : entity work.mtnc
    port map (awuiplcik => zw, jpk => tfssztzlgb, d => utonrswii);
end cctsbsvtn;

library ieee;
use ieee.std_logic_1164.all;

entity ohjp is
  port (zktoxpbn : out std_logic; vnqhkjbsy : in std_logic; mrkokcg : out time);
end ohjp;

library ieee;
use ieee.std_logic_1164.all;

architecture vggclmksdb of ohjp is
  signal pqbyl : std_logic;
  signal pk : std_logic;
  signal oyzayp : real;
  signal eea : time;
begin
  sracfs : entity work.mtnc
    port map (awuiplcik => vnqhkjbsy, jpk => eea, d => oyzayp);
  reth : entity work.mtnc
    port map (awuiplcik => pk, jpk => mrkokcg, d => oyzayp);
  irig : entity work.arcn
    port map (kd => pqbyl);
end vggclmksdb;



-- Seed after: 15084144977442824555,17611625116949931313
