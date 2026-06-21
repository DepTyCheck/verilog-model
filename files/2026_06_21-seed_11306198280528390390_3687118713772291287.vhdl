-- Seed: 11306198280528390390,3687118713772291287

library ieee;
use ieee.std_logic_1164.all;

entity atzpzx is
  port (vobcbjm : buffer integer_vector(1 downto 0); jpsrc : out std_logic_vector(0 to 1); veqqnmkm : linkage std_logic; frktig : buffer real);
end atzpzx;

architecture ohizcc of atzpzx is
  
begin
  -- Single-driven assignments
  vobcbjm <= (2, 2_2_1_4);
  
  -- Multi-driven assignments
  jpsrc <= "0W";
  jpsrc <= "UH";
  jpsrc <= "0-";
end ohizcc;

entity ylwvcg is
  port (eedvwqn : buffer string(3 to 1); vdfjqwcm : out real_vector(0 downto 3));
end ylwvcg;

library ieee;
use ieee.std_logic_1164.all;

architecture jxfmqw of ylwvcg is
  signal lifrp : real;
  signal dxxjdsxpu : std_logic;
  signal fm : std_logic_vector(0 to 1);
  signal yxqvsczjl : integer_vector(1 downto 0);
  signal hp : real;
  signal yjd : std_logic;
  signal qnfuqacszz : integer_vector(1 downto 0);
  signal krykf : real;
  signal kclossoss : std_logic;
  signal nkwzopri : std_logic_vector(0 to 1);
  signal xtqyyf : integer_vector(1 downto 0);
begin
  rd : entity work.atzpzx
    port map (vobcbjm => xtqyyf, jpsrc => nkwzopri, veqqnmkm => kclossoss, frktig => krykf);
  fpwq : entity work.atzpzx
    port map (vobcbjm => qnfuqacszz, jpsrc => nkwzopri, veqqnmkm => yjd, frktig => hp);
  hooadg : entity work.atzpzx
    port map (vobcbjm => yxqvsczjl, jpsrc => fm, veqqnmkm => dxxjdsxpu, frktig => lifrp);
  
  -- Single-driven assignments
  eedvwqn <= (others => ' ');
end jxfmqw;



-- Seed after: 14670007685712375737,3687118713772291287
