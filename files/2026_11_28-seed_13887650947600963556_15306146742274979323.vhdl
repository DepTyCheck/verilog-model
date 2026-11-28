-- Seed: 13887650947600963556,15306146742274979323

library ieee;
use ieee.std_logic_1164.all;

entity xtrfvixg is
  port (wpgdxbdl : out integer; bvtbhurih : buffer bit_vector(3 downto 0); owvqyqtn : buffer std_logic_vector(1 to 1); sdy : buffer string(3 downto 2));
end xtrfvixg;

architecture lapuek of xtrfvixg is
  
begin
  -- Single-driven assignments
  bvtbhurih <= ('1', '1', '1', '0');
  wpgdxbdl <= 0_3;
  sdy <= "do";
end lapuek;

entity yq is
  port (p : buffer real_vector(0 to 4); qgml : inout time);
end yq;

library ieee;
use ieee.std_logic_1164.all;

architecture hubnhw of yq is
  signal zda : string(3 downto 2);
  signal wfvcxweezq : bit_vector(3 downto 0);
  signal d : integer;
  signal btvugv : string(3 downto 2);
  signal kqckuhqn : std_logic_vector(1 to 1);
  signal paymetjfr : bit_vector(3 downto 0);
  signal hqw : integer;
  signal z : string(3 downto 2);
  signal kdhhxw : bit_vector(3 downto 0);
  signal ouep : integer;
  signal mhp : string(3 downto 2);
  signal qnh : std_logic_vector(1 to 1);
  signal jhysnqrf : bit_vector(3 downto 0);
  signal yjiqg : integer;
begin
  jra : entity work.xtrfvixg
    port map (wpgdxbdl => yjiqg, bvtbhurih => jhysnqrf, owvqyqtn => qnh, sdy => mhp);
  qmppqjcrsm : entity work.xtrfvixg
    port map (wpgdxbdl => ouep, bvtbhurih => kdhhxw, owvqyqtn => qnh, sdy => z);
  memcfelgk : entity work.xtrfvixg
    port map (wpgdxbdl => hqw, bvtbhurih => paymetjfr, owvqyqtn => kqckuhqn, sdy => btvugv);
  ytbjtz : entity work.xtrfvixg
    port map (wpgdxbdl => d, bvtbhurih => wfvcxweezq, owvqyqtn => qnh, sdy => zda);
  
  -- Multi-driven assignments
  kqckuhqn <= qnh;
end hubnhw;



-- Seed after: 4443359994277308203,15306146742274979323
