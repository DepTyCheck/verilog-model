-- Seed: 5849042320374511808,12569788116744667457

library ieee;
use ieee.std_logic_1164.all;

entity wkmxxzbg is
  port (lnscxxzb : out std_logic; xmdnues : inout std_logic; kscr : buffer time; vk : out real);
end wkmxxzbg;



architecture qyjjgdb of wkmxxzbg is
  
begin
  
end qyjjgdb;

library ieee;
use ieee.std_logic_1164.all;

entity ezs is
  port (xckdzlcpfq : in std_logic);
end ezs;

library ieee;
use ieee.std_logic_1164.all;

architecture dlb of ezs is
  signal qvc : real;
  signal veyfefbin : time;
  signal cin : std_logic;
begin
  oaep : entity work.wkmxxzbg
    port map (lnscxxzb => cin, xmdnues => cin, kscr => veyfefbin, vk => qvc);
end dlb;

library ieee;
use ieee.std_logic_1164.all;

entity pknqc is
  port (mvengmrupg : linkage std_logic);
end pknqc;

library ieee;
use ieee.std_logic_1164.all;

architecture einqj of pknqc is
  signal fieilarl : real;
  signal mg : time;
  signal mh : std_logic;
  signal mdwfx : std_logic;
begin
  qppyyw : entity work.wkmxxzbg
    port map (lnscxxzb => mdwfx, xmdnues => mh, kscr => mg, vk => fieilarl);
end einqj;

library ieee;
use ieee.std_logic_1164.all;

entity vztbmn is
  port (pkziww : linkage std_logic; ycynk : in character);
end vztbmn;

library ieee;
use ieee.std_logic_1164.all;

architecture tmwiu of vztbmn is
  signal uul : std_logic;
  signal taeqx : std_logic;
begin
  txk : entity work.ezs
    port map (xckdzlcpfq => taeqx);
  ervtsshz : entity work.pknqc
    port map (mvengmrupg => uul);
  toxo : entity work.pknqc
    port map (mvengmrupg => pkziww);
end tmwiu;



-- Seed after: 11588114681694680557,12569788116744667457
