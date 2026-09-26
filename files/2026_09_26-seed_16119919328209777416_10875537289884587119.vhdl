-- Seed: 16119919328209777416,10875537289884587119

library ieee;
use ieee.std_logic_1164.all;

entity fuagu is
  port (tuzc : out boolean; robelgij : buffer time; vgkvqat : linkage real; usylosybe : out std_logic);
end fuagu;

architecture jxl of fuagu is
  
begin
  -- Single-driven assignments
  robelgij <= 4_1 ms;
  tuzc <= FALSE;
end jxl;

library ieee;
use ieee.std_logic_1164.all;

entity vlndaxsqfg is
  port (zo : linkage time; vtyaiigmiw : out std_logic; ahbfvkichx : buffer std_logic; eilok : in time);
end vlndaxsqfg;

library ieee;
use ieee.std_logic_1164.all;

architecture k of vlndaxsqfg is
  signal egvimlty : std_logic;
  signal fmlhmhta : real;
  signal w : time;
  signal jmqrlj : boolean;
  signal bddcjsvf : real;
  signal x : time;
  signal kcvsk : boolean;
  signal nr : real;
  signal pduib : time;
  signal gggbmjei : boolean;
begin
  otubz : entity work.fuagu
    port map (tuzc => gggbmjei, robelgij => pduib, vgkvqat => nr, usylosybe => ahbfvkichx);
  dar : entity work.fuagu
    port map (tuzc => kcvsk, robelgij => x, vgkvqat => bddcjsvf, usylosybe => ahbfvkichx);
  vubx : entity work.fuagu
    port map (tuzc => jmqrlj, robelgij => w, vgkvqat => fmlhmhta, usylosybe => egvimlty);
  
  -- Multi-driven assignments
  ahbfvkichx <= 'L';
  egvimlty <= 'U';
end k;

entity scueq is
  port (cj : out real);
end scueq;

library ieee;
use ieee.std_logic_1164.all;

architecture rrsrpmlogh of scueq is
  signal y : std_logic;
  signal zmydbubstk : time;
  signal gdkxo : boolean;
  signal sn : std_logic;
  signal kwpow : real;
  signal skkqdt : time;
  signal bpjabhbt : boolean;
begin
  fuhlhw : entity work.fuagu
    port map (tuzc => bpjabhbt, robelgij => skkqdt, vgkvqat => kwpow, usylosybe => sn);
  gfu : entity work.fuagu
    port map (tuzc => gdkxo, robelgij => zmydbubstk, vgkvqat => cj, usylosybe => y);
  
  -- Multi-driven assignments
  y <= sn;
  sn <= sn;
end rrsrpmlogh;

library ieee;
use ieee.std_logic_1164.all;

entity vlsk is
  port (yntmvicp : linkage std_logic; hxoaspuad : inout boolean_vector(0 downto 3); mvorsykg : in time; qonmsi : out bit);
end vlsk;

library ieee;
use ieee.std_logic_1164.all;

architecture lnephjcx of vlsk is
  signal uao : std_logic;
  signal dwehktut : real;
  signal zbbynmt : time;
  signal tmgsvr : boolean;
  signal iutijms : std_logic;
  signal mogyoewtx : real;
  signal zpqrdbceag : time;
  signal wyfwqxfi : boolean;
  signal dnhzgxcp : std_logic;
  signal smqnrenam : real;
  signal vhepqzclfd : time;
  signal hhdy : boolean;
begin
  yv : entity work.fuagu
    port map (tuzc => hhdy, robelgij => vhepqzclfd, vgkvqat => smqnrenam, usylosybe => dnhzgxcp);
  uoimpoe : entity work.fuagu
    port map (tuzc => wyfwqxfi, robelgij => zpqrdbceag, vgkvqat => mogyoewtx, usylosybe => iutijms);
  satdj : entity work.fuagu
    port map (tuzc => tmgsvr, robelgij => zbbynmt, vgkvqat => dwehktut, usylosybe => uao);
  
  -- Single-driven assignments
  qonmsi <= qonmsi;
  hxoaspuad <= (others => TRUE);
  
  -- Multi-driven assignments
  dnhzgxcp <= 'W';
  iutijms <= dnhzgxcp;
end lnephjcx;



-- Seed after: 3923606516370285420,10875537289884587119
