-- Seed: 1328484851320120990,15300320181035395489

library ieee;
use ieee.std_logic_1164.all;

entity af is
  port (ta : linkage std_logic_vector(1 downto 0); rrp : in real; bjc : in time; hjnet : in std_logic_vector(4 downto 3));
end af;

architecture w of af is
  
begin
  
end w;

entity hknvbusdvo is
  port (ip : in time);
end hknvbusdvo;

library ieee;
use ieee.std_logic_1164.all;

architecture foo of hknvbusdvo is
  signal dxptxfll : real;
  signal ms : std_logic_vector(4 downto 3);
  signal hnwuwrkm : real;
  signal ggudnqffo : std_logic_vector(1 downto 0);
  signal q : std_logic_vector(4 downto 3);
  signal qjwl : time;
  signal v : std_logic_vector(1 downto 0);
  signal dspv : time;
  signal zsrbumo : real;
  signal whevezw : std_logic_vector(4 downto 3);
begin
  d : entity work.af
    port map (ta => whevezw, rrp => zsrbumo, bjc => dspv, hjnet => whevezw);
  hbzdmt : entity work.af
    port map (ta => v, rrp => zsrbumo, bjc => qjwl, hjnet => q);
  nhwjbeenii : entity work.af
    port map (ta => ggudnqffo, rrp => hnwuwrkm, bjc => dspv, hjnet => ms);
  pntwpcuz : entity work.af
    port map (ta => whevezw, rrp => dxptxfll, bjc => ip, hjnet => whevezw);
  
  -- Single-driven assignments
  dxptxfll <= 3.0;
  
  -- Multi-driven assignments
  ms <= "10";
  whevezw <= "X-";
  v <= ('U', '0');
  whevezw <= ('X', '1');
end foo;



-- Seed after: 8352051345487945688,15300320181035395489
