-- Seed: 9928685938448090160,8421704836678237495

library ieee;
use ieee.std_logic_1164.all;

entity hoelnsv is
  port (jjtazgv : linkage std_logic; l : buffer bit_vector(4 to 0); ci : inout boolean; swguvrjic : in integer_vector(3 to 1));
end hoelnsv;

architecture ubbnt of hoelnsv is
  
begin
  -- Single-driven assignments
  ci <= FALSE;
  l <= (others => '0');
end ubbnt;

library ieee;
use ieee.std_logic_1164.all;

entity bavfjln is
  port (tpatmdccw : linkage boolean; vnprngjwaq : buffer std_logic; c : out integer_vector(4 to 4));
end bavfjln;

library ieee;
use ieee.std_logic_1164.all;

architecture yblib of bavfjln is
  signal fleu : integer_vector(3 to 1);
  signal sskhnclkcg : boolean;
  signal gl : bit_vector(4 to 0);
  signal pbdazt : boolean;
  signal zu : bit_vector(4 to 0);
  signal joccxlu : integer_vector(3 to 1);
  signal luvj : boolean;
  signal edxzn : bit_vector(4 to 0);
  signal zk : integer_vector(3 to 1);
  signal bjko : boolean;
  signal nfi : bit_vector(4 to 0);
  signal wtwqdlejlw : std_logic;
begin
  vqbygsap : entity work.hoelnsv
    port map (jjtazgv => wtwqdlejlw, l => nfi, ci => bjko, swguvrjic => zk);
  pjij : entity work.hoelnsv
    port map (jjtazgv => vnprngjwaq, l => edxzn, ci => luvj, swguvrjic => joccxlu);
  alhky : entity work.hoelnsv
    port map (jjtazgv => vnprngjwaq, l => zu, ci => pbdazt, swguvrjic => joccxlu);
  xrgkcuwtk : entity work.hoelnsv
    port map (jjtazgv => vnprngjwaq, l => gl, ci => sskhnclkcg, swguvrjic => fleu);
  
  -- Single-driven assignments
  zk <= (others => 0);
  fleu <= (others => 0);
  joccxlu <= (others => 0);
  c <= (others => 1_4);
  
  -- Multi-driven assignments
  vnprngjwaq <= 'W';
  wtwqdlejlw <= 'H';
  vnprngjwaq <= 'U';
  vnprngjwaq <= 'U';
end yblib;

entity nxawglw is
  port (afqewy : in time; dk : linkage real; ggjn : in real);
end nxawglw;

library ieee;
use ieee.std_logic_1164.all;

architecture kptabv of nxawglw is
  signal oj : integer_vector(3 to 1);
  signal xnfkfgsye : boolean;
  signal sdcgh : bit_vector(4 to 0);
  signal ferwtum : std_logic;
  signal kawfh : integer_vector(4 to 4);
  signal bdv : std_logic;
  signal bp : boolean;
begin
  q : entity work.bavfjln
    port map (tpatmdccw => bp, vnprngjwaq => bdv, c => kawfh);
  znyrnbfo : entity work.hoelnsv
    port map (jjtazgv => ferwtum, l => sdcgh, ci => xnfkfgsye, swguvrjic => oj);
  
  -- Single-driven assignments
  oj <= (others => 0);
  
  -- Multi-driven assignments
  bdv <= 'X';
  bdv <= '0';
  bdv <= '1';
end kptabv;

entity tbmyobmyyo is
  port (qjtq : out boolean; jvzjvjta : out real);
end tbmyobmyyo;

library ieee;
use ieee.std_logic_1164.all;

architecture dpn of tbmyobmyyo is
  signal dfb : integer_vector(3 to 1);
  signal ttww : bit_vector(4 to 0);
  signal acxbgkvvvs : std_logic;
begin
  jyvlb : entity work.hoelnsv
    port map (jjtazgv => acxbgkvvvs, l => ttww, ci => qjtq, swguvrjic => dfb);
  
  -- Single-driven assignments
  jvzjvjta <= 2#1_1_1_1.1#;
  
  -- Multi-driven assignments
  acxbgkvvvs <= 'Z';
end dpn;



-- Seed after: 1909388271978576067,8421704836678237495
