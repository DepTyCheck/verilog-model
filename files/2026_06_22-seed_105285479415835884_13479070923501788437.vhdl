-- Seed: 105285479415835884,13479070923501788437

library ieee;
use ieee.std_logic_1164.all;

entity gizewnlk is
  port (rwkcm : in real; yvx : out std_logic; uadnheevzb : in integer; i : in std_logic_vector(0 downto 4));
end gizewnlk;

architecture zneuqc of gizewnlk is
  
begin
  -- Multi-driven assignments
  yvx <= 'H';
end zneuqc;

library ieee;
use ieee.std_logic_1164.all;

entity xkoo is
  port (bhbq : in std_logic_vector(4 to 2); gq : inout std_logic; oeazywhrs : in real_vector(1 downto 1); zuytcdevbm : linkage std_logic);
end xkoo;

library ieee;
use ieee.std_logic_1164.all;

architecture yuveq of xkoo is
  signal jo : std_logic_vector(0 downto 4);
  signal nfkoear : integer;
  signal vaaz : std_logic;
  signal y : std_logic_vector(0 downto 4);
  signal zw : integer;
  signal ndlnlamcj : real;
begin
  mbgz : entity work.gizewnlk
    port map (rwkcm => ndlnlamcj, yvx => gq, uadnheevzb => zw, i => y);
  odxnmbj : entity work.gizewnlk
    port map (rwkcm => ndlnlamcj, yvx => vaaz, uadnheevzb => nfkoear, i => jo);
  
  -- Single-driven assignments
  nfkoear <= 0_2_1_1;
  ndlnlamcj <= 0_0_2_1_2.4324;
end yuveq;

entity rrkuvxch is
  port (efdcpd : linkage time; shidfrqg : buffer real_vector(2 downto 1); bhhx : in time; j : in integer);
end rrkuvxch;

library ieee;
use ieee.std_logic_1164.all;

architecture wfpmbke of rrkuvxch is
  signal cygg : std_logic_vector(0 downto 4);
  signal vkvsqowi : integer;
  signal ul : std_logic;
  signal vac : real;
begin
  kdvce : entity work.gizewnlk
    port map (rwkcm => vac, yvx => ul, uadnheevzb => vkvsqowi, i => cygg);
  
  -- Single-driven assignments
  vkvsqowi <= 16#BC6#;
  vac <= 8#2.32#;
  shidfrqg <= (01.1, 4_0_1_0_4.40002);
  
  -- Multi-driven assignments
  cygg <= (others => '0');
  ul <= 'L';
  ul <= 'U';
end wfpmbke;

entity bdanfqm is
  port (rqsvj : inout real);
end bdanfqm;

library ieee;
use ieee.std_logic_1164.all;

architecture aixsn of bdanfqm is
  signal xexsvasw : real;
  signal gkk : integer;
  signal d : std_logic;
  signal n : real;
  signal v : std_logic;
  signal wwggqovifz : real_vector(1 downto 1);
  signal nsosz : std_logic;
  signal ozzguoni : std_logic_vector(0 downto 4);
begin
  dhbvhssdhz : entity work.xkoo
    port map (bhbq => ozzguoni, gq => nsosz, oeazywhrs => wwggqovifz, zuytcdevbm => v);
  fns : entity work.gizewnlk
    port map (rwkcm => n, yvx => d, uadnheevzb => gkk, i => ozzguoni);
  lb : entity work.gizewnlk
    port map (rwkcm => xexsvasw, yvx => d, uadnheevzb => gkk, i => ozzguoni);
  
  -- Single-driven assignments
  xexsvasw <= 2#1_0.1_1#;
  
  -- Multi-driven assignments
  d <= 'U';
  d <= 'L';
  nsosz <= 'X';
end aixsn;



-- Seed after: 16377354815422910608,13479070923501788437
