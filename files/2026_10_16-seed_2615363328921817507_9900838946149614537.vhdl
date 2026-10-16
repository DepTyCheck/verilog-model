-- Seed: 2615363328921817507,9900838946149614537

library ieee;
use ieee.std_logic_1164.all;

entity bpkdvpp is
  port (febexc : linkage integer_vector(0 to 4); iuvyjjb : linkage std_logic; nsjoek : out integer; nzlbjxgof : in integer);
end bpkdvpp;

architecture ofmhnxlpi of bpkdvpp is
  
begin
  
end ofmhnxlpi;

entity xxytmqm is
  port (zwozoaf : linkage time; wkcky : buffer integer; t : inout integer; krdag : buffer real);
end xxytmqm;

architecture mvyuxrlwpo of xxytmqm is
  
begin
  -- Single-driven assignments
  krdag <= 16#1E.DC5#;
  wkcky <= wkcky;
  t <= wkcky;
end mvyuxrlwpo;

library ieee;
use ieee.std_logic_1164.all;

entity xtnbkxmwvo is
  port (ydlqgti : inout string(2 to 3); esnd : buffer std_logic; eicpkfc : out integer_vector(0 to 3); m : inout time);
end xtnbkxmwvo;

architecture y of xtnbkxmwvo is
  signal n : real;
  signal wg : integer;
  signal qaioi : integer;
  signal ujerps : time;
  signal iew : integer;
  signal bzzpngrn : integer_vector(0 to 4);
  signal ceqbcnb : integer;
  signal vcqtqq : integer;
  signal kllog : integer_vector(0 to 4);
begin
  syugqi : entity work.bpkdvpp
    port map (febexc => kllog, iuvyjjb => esnd, nsjoek => vcqtqq, nzlbjxgof => ceqbcnb);
  qglsj : entity work.bpkdvpp
    port map (febexc => bzzpngrn, iuvyjjb => esnd, nsjoek => ceqbcnb, nzlbjxgof => iew);
  iq : entity work.xxytmqm
    port map (zwozoaf => ujerps, wkcky => qaioi, t => wg, krdag => n);
  
  -- Single-driven assignments
  m <= 0 min;
  
  -- Multi-driven assignments
  esnd <= esnd;
  esnd <= esnd;
  esnd <= 'H';
end y;

entity xugpzgvfrv is
  port (nurzzy : out real; hh : linkage integer; zdrfjhyt : inout time);
end xugpzgvfrv;

library ieee;
use ieee.std_logic_1164.all;

architecture rjek of xugpzgvfrv is
  signal x : integer_vector(0 to 3);
  signal ys : std_logic;
  signal vl : string(2 to 3);
  signal jkfzqyhzo : integer;
  signal smy : time;
  signal nkthra : integer;
  signal eofggx : integer;
  signal wow : std_logic;
  signal uxcrvsnxmq : integer_vector(0 to 4);
begin
  fzkkx : entity work.bpkdvpp
    port map (febexc => uxcrvsnxmq, iuvyjjb => wow, nsjoek => eofggx, nzlbjxgof => nkthra);
  uohmyqeav : entity work.xxytmqm
    port map (zwozoaf => smy, wkcky => jkfzqyhzo, t => nkthra, krdag => nurzzy);
  u : entity work.xtnbkxmwvo
    port map (ydlqgti => vl, esnd => ys, eicpkfc => x, m => zdrfjhyt);
  
  -- Multi-driven assignments
  wow <= '0';
end rjek;



-- Seed after: 6852610878763805166,9900838946149614537
