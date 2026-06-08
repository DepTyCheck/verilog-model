-- Seed: 1008782902326074244,7142793346053417159

library ieee;
use ieee.std_logic_1164.all;

entity fd is
  port (ncdsciv : buffer std_logic_vector(4 downto 1); swth : inout std_logic_vector(2 downto 3); ly : buffer time_vector(0 to 0));
end fd;



architecture e of fd is
  
begin
  
end e;

library ieee;
use ieee.std_logic_1164.all;

entity lym is
  port (x : inout std_logic; ogduseztew : linkage std_logic);
end lym;

library ieee;
use ieee.std_logic_1164.all;

architecture tpxxxbxd of lym is
  signal nifr : time_vector(0 to 0);
  signal v : std_logic_vector(2 downto 3);
  signal vuvsf : std_logic_vector(4 downto 1);
begin
  ac : entity work.fd
    port map (ncdsciv => vuvsf, swth => v, ly => nifr);
end tpxxxbxd;

library ieee;
use ieee.std_logic_1164.all;

entity zmnsrgjjw is
  port (mqxwwy : linkage real; frgyxyhyb : linkage std_logic_vector(4 downto 4));
end zmnsrgjjw;

library ieee;
use ieee.std_logic_1164.all;

architecture itfruznbc of zmnsrgjjw is
  signal vhtowjd : time_vector(0 to 0);
  signal e : std_logic;
  signal jwincbfxrq : std_logic;
  signal iooq : time_vector(0 to 0);
  signal fvuqbpx : std_logic_vector(2 downto 3);
  signal tjcrwobd : std_logic_vector(4 downto 1);
begin
  isbxwlvw : entity work.fd
    port map (ncdsciv => tjcrwobd, swth => fvuqbpx, ly => iooq);
  ckel : entity work.lym
    port map (x => jwincbfxrq, ogduseztew => e);
  lgele : entity work.fd
    port map (ncdsciv => tjcrwobd, swth => fvuqbpx, ly => vhtowjd);
end itfruznbc;



entity qaxosblgw is
  port (kyobn : in integer; lozfwilons : linkage bit_vector(4 to 2); vdbgez : in severity_level);
end qaxosblgw;

library ieee;
use ieee.std_logic_1164.all;

architecture iqhs of qaxosblgw is
  signal jwzajppkk : std_logic_vector(4 downto 4);
  signal mwaiqmn : real;
  signal vvlyoregde : time_vector(0 to 0);
  signal yhphggwtm : std_logic_vector(2 downto 3);
  signal kfbkoczen : std_logic_vector(4 downto 1);
  signal spgncisgaz : std_logic;
  signal wqsic : time_vector(0 to 0);
  signal nkqpsz : std_logic_vector(2 downto 3);
  signal adi : std_logic_vector(4 downto 1);
begin
  tpspxfktlh : entity work.fd
    port map (ncdsciv => adi, swth => nkqpsz, ly => wqsic);
  by : entity work.lym
    port map (x => spgncisgaz, ogduseztew => spgncisgaz);
  mih : entity work.fd
    port map (ncdsciv => kfbkoczen, swth => yhphggwtm, ly => vvlyoregde);
  nwl : entity work.zmnsrgjjw
    port map (mqxwwy => mwaiqmn, frgyxyhyb => jwzajppkk);
end iqhs;



-- Seed after: 7728608737264427079,7142793346053417159
