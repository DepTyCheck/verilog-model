-- Seed: 2219489672968617345,10975587770517081373

entity qzc is
  port (zxylk : buffer real_vector(3 downto 2); j : out real);
end qzc;

architecture anojzpryt of qzc is
  
begin
  -- Single-driven assignments
  zxylk <= zxylk;
end anojzpryt;

entity iudug is
  port (hjiqa : linkage integer; xacju : out real; kqzjyv : inout time; di : inout time);
end iudug;

architecture mtvvy of iudug is
  signal su : real_vector(3 downto 2);
  signal hmuhexvaxm : real;
  signal gbxwidzm : real_vector(3 downto 2);
  signal atf : real;
  signal a : real_vector(3 downto 2);
begin
  rarr : entity work.qzc
    port map (zxylk => a, j => atf);
  b : entity work.qzc
    port map (zxylk => gbxwidzm, j => hmuhexvaxm);
  azpijyw : entity work.qzc
    port map (zxylk => su, j => xacju);
end mtvvy;

library ieee;
use ieee.std_logic_1164.all;

entity lijn is
  port (rwpzwy : in time; kzlohfuue : in std_logic_vector(1 downto 2); r : in std_logic; pvpihlseho : linkage bit);
end lijn;

architecture xhcgdwrni of lijn is
  signal maldwbbf : time;
  signal zefc : time;
  signal fhsvkawj : real;
  signal kvnyul : integer;
begin
  kicfpg : entity work.iudug
    port map (hjiqa => kvnyul, xacju => fhsvkawj, kqzjyv => zefc, di => maldwbbf);
end xhcgdwrni;

entity jwe is
  port (efrnf : out integer_vector(3 to 4));
end jwe;

architecture okhefgjf of jwe is
  signal snjydaka : real;
  signal sfrc : real_vector(3 downto 2);
  signal varcixul : time;
  signal cpeygsfpy : time;
  signal hfxvdh : real;
  signal xofgbyk : integer;
  signal t : time;
  signal h : time;
  signal wcfxy : real;
  signal jdkvvsitb : integer;
  signal ngvapvxrws : time;
  signal ntjqxezl : time;
  signal hfqex : real;
  signal oiearil : integer;
begin
  johqx : entity work.iudug
    port map (hjiqa => oiearil, xacju => hfqex, kqzjyv => ntjqxezl, di => ngvapvxrws);
  yxkgxwa : entity work.iudug
    port map (hjiqa => jdkvvsitb, xacju => wcfxy, kqzjyv => h, di => t);
  wk : entity work.iudug
    port map (hjiqa => xofgbyk, xacju => hfxvdh, kqzjyv => cpeygsfpy, di => varcixul);
  huvh : entity work.qzc
    port map (zxylk => sfrc, j => snjydaka);
end okhefgjf;



-- Seed after: 9155314045254591208,10975587770517081373
