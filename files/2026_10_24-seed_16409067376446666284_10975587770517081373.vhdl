-- Seed: 16409067376446666284,10975587770517081373

library ieee;
use ieee.std_logic_1164.all;

entity zvclooapd is
  port ( i : buffer std_logic_vector(2 downto 3)
  ; szbkegc : inout real_vector(1 downto 1)
  ; xrqvhhymli : out std_logic_vector(0 to 1)
  ; qywbjwbvul : buffer time_vector(3 to 4)
  );
end zvclooapd;

architecture xebtdjzio of zvclooapd is
  
begin
  -- Single-driven assignments
  qywbjwbvul <= (2#1_0_1.01# ps, 8#353# us);
  szbkegc <= szbkegc;
end xebtdjzio;

library ieee;
use ieee.std_logic_1164.all;

entity crvsmnu is
  port (z : buffer std_logic);
end crvsmnu;

library ieee;
use ieee.std_logic_1164.all;

architecture zwb of crvsmnu is
  signal syxrr : time_vector(3 to 4);
  signal p : real_vector(1 downto 1);
  signal nyonh : time_vector(3 to 4);
  signal kywwv : std_logic_vector(0 to 1);
  signal tumyeuidi : real_vector(1 downto 1);
  signal tpyjo : std_logic_vector(2 downto 3);
begin
  czwkjoa : entity work.zvclooapd
    port map (i => tpyjo, szbkegc => tumyeuidi, xrqvhhymli => kywwv, qywbjwbvul => nyonh);
  foq : entity work.zvclooapd
    port map (i => tpyjo, szbkegc => p, xrqvhhymli => kywwv, qywbjwbvul => syxrr);
end zwb;

library ieee;
use ieee.std_logic_1164.all;

entity xiqcbe is
  port (vvrxdtbls : in std_logic; aslebe : inout integer; ahmeoz : out std_logic_vector(2 downto 1));
end xiqcbe;

library ieee;
use ieee.std_logic_1164.all;

architecture ztobw of xiqcbe is
  signal yxhm : time_vector(3 to 4);
  signal cuthhdtid : real_vector(1 downto 1);
  signal ijm : std_logic_vector(2 downto 3);
  signal ssorstspot : time_vector(3 to 4);
  signal icyeud : real_vector(1 downto 1);
  signal oa : time_vector(3 to 4);
  signal sc : std_logic_vector(0 to 1);
  signal zwqektf : real_vector(1 downto 1);
  signal qy : time_vector(3 to 4);
  signal beflet : real_vector(1 downto 1);
  signal csfltyrgoa : std_logic_vector(2 downto 3);
begin
  pc : entity work.zvclooapd
    port map (i => csfltyrgoa, szbkegc => beflet, xrqvhhymli => ahmeoz, qywbjwbvul => qy);
  pzsvbjt : entity work.zvclooapd
    port map (i => csfltyrgoa, szbkegc => zwqektf, xrqvhhymli => sc, qywbjwbvul => oa);
  vwoyjid : entity work.zvclooapd
    port map (i => csfltyrgoa, szbkegc => icyeud, xrqvhhymli => ahmeoz, qywbjwbvul => ssorstspot);
  s : entity work.zvclooapd
    port map (i => ijm, szbkegc => cuthhdtid, xrqvhhymli => sc, qywbjwbvul => yxhm);
  
  -- Single-driven assignments
  aslebe <= aslebe;
  
  -- Multi-driven assignments
  ahmeoz <= ahmeoz;
  ijm <= (others => '0');
end ztobw;

library ieee;
use ieee.std_logic_1164.all;

entity tpjsngqp is
  port (mqol : in severity_level; slposk : out std_logic_vector(0 to 4));
end tpjsngqp;

library ieee;
use ieee.std_logic_1164.all;

architecture pcwl of tpjsngqp is
  signal k : time_vector(3 to 4);
  signal jhoa : std_logic_vector(0 to 1);
  signal hdkpbxd : real_vector(1 downto 1);
  signal tzmefhr : std_logic_vector(2 downto 3);
begin
  ufinr : entity work.zvclooapd
    port map (i => tzmefhr, szbkegc => hdkpbxd, xrqvhhymli => jhoa, qywbjwbvul => k);
  
  -- Multi-driven assignments
  slposk <= slposk;
end pcwl;



-- Seed after: 8239701520406792713,10975587770517081373
