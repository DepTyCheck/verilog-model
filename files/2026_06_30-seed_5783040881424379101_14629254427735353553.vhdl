-- Seed: 5783040881424379101,14629254427735353553

library ieee;
use ieee.std_logic_1164.all;

entity tzotuulebb is
  port (jhxhll : buffer std_logic_vector(1 downto 4); yyb : in integer; tioipx : in std_logic_vector(2 downto 0));
end tzotuulebb;

architecture tiqk of tzotuulebb is
  
begin
  
end tiqk;

entity oyqn is
  port (foirisgf : linkage real; yqzy : out boolean_vector(1 to 3));
end oyqn;

library ieee;
use ieee.std_logic_1164.all;

architecture dmdoebfwne of oyqn is
  signal oflk : std_logic_vector(2 downto 0);
  signal hzeaqcmdb : integer;
  signal jggswik : integer;
  signal uszbutjqen : std_logic_vector(1 downto 4);
  signal qjk : std_logic_vector(2 downto 0);
  signal yzer : integer;
  signal wumm : std_logic_vector(1 downto 4);
  signal waar : std_logic_vector(2 downto 0);
  signal gvxeb : integer;
  signal vqjbp : std_logic_vector(1 downto 4);
begin
  xmwuixkx : entity work.tzotuulebb
    port map (jhxhll => vqjbp, yyb => gvxeb, tioipx => waar);
  kxqhvub : entity work.tzotuulebb
    port map (jhxhll => wumm, yyb => yzer, tioipx => qjk);
  qnwvgvv : entity work.tzotuulebb
    port map (jhxhll => uszbutjqen, yyb => jggswik, tioipx => qjk);
  ctki : entity work.tzotuulebb
    port map (jhxhll => vqjbp, yyb => hzeaqcmdb, tioipx => oflk);
  
  -- Single-driven assignments
  yqzy <= (FALSE, FALSE, TRUE);
  gvxeb <= 0401;
  jggswik <= 8#4_1_2_0_1#;
  
  -- Multi-driven assignments
  qjk <= "W0H";
  vqjbp <= (others => '0');
end dmdoebfwne;

entity uyxgoyjy is
  port (xi : linkage boolean_vector(3 downto 3); sluni : in real);
end uyxgoyjy;

library ieee;
use ieee.std_logic_1164.all;

architecture vmlmkek of uyxgoyjy is
  signal fnfykxx : std_logic_vector(2 downto 0);
  signal sbrcczu : integer;
  signal sxncxk : std_logic_vector(1 downto 4);
begin
  w : entity work.tzotuulebb
    port map (jhxhll => sxncxk, yyb => sbrcczu, tioipx => fnfykxx);
end vmlmkek;



-- Seed after: 9400183902567812017,14629254427735353553
