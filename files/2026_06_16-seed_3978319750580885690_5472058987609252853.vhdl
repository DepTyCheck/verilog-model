-- Seed: 3978319750580885690,5472058987609252853

entity vfa is
  port (nxpcxvm : linkage real_vector(2 to 4); enwbppf : linkage real; iqybpqeuth : linkage time);
end vfa;

architecture cycvnzhr of vfa is
  
begin
  
end cycvnzhr;

entity du is
  port (skj : inout real; qyqwuly : buffer integer; sbxpybiqn : linkage character; kligiilvf : out integer);
end du;

architecture awnwspyei of du is
  
begin
  
end awnwspyei;

library ieee;
use ieee.std_logic_1164.all;

entity lugrzlxf is
  port (yq : inout time; culoxsdsow : inout bit; op : out std_logic);
end lugrzlxf;

architecture bawel of lugrzlxf is
  signal ttbvyh : real;
  signal srqtw : real_vector(2 to 4);
  signal bumlrx : time;
  signal csntegpri : real;
  signal qgjtfq : real_vector(2 to 4);
  signal rt : integer;
  signal wxbatpjba : character;
  signal btvxtbfue : integer;
  signal oy : real;
  signal u : integer;
  signal xkjio : character;
  signal lssqyqvbzl : integer;
  signal plmuyk : real;
begin
  zina : entity work.du
    port map (skj => plmuyk, qyqwuly => lssqyqvbzl, sbxpybiqn => xkjio, kligiilvf => u);
  xbvkr : entity work.du
    port map (skj => oy, qyqwuly => btvxtbfue, sbxpybiqn => wxbatpjba, kligiilvf => rt);
  imcgahza : entity work.vfa
    port map (nxpcxvm => qgjtfq, enwbppf => csntegpri, iqybpqeuth => bumlrx);
  mocot : entity work.vfa
    port map (nxpcxvm => srqtw, enwbppf => ttbvyh, iqybpqeuth => yq);
  
  -- Single-driven assignments
  culoxsdsow <= '0';
  
  -- Multi-driven assignments
  op <= 'Z';
  op <= 'U';
  op <= 'X';
end bawel;

entity gi is
  port (tvat : linkage bit);
end gi;

architecture fsk of gi is
  signal pkdwa : time;
  signal eroaknqhl : real;
  signal lcndwzg : real_vector(2 to 4);
  signal qsyc : time;
  signal xgsooqur : real;
  signal hcudpajavj : real_vector(2 to 4);
  signal mzmmpn : integer;
  signal goasw : character;
  signal ivpm : integer;
  signal ijmanctd : real;
begin
  ikudygik : entity work.du
    port map (skj => ijmanctd, qyqwuly => ivpm, sbxpybiqn => goasw, kligiilvf => mzmmpn);
  uw : entity work.vfa
    port map (nxpcxvm => hcudpajavj, enwbppf => xgsooqur, iqybpqeuth => qsyc);
  csjg : entity work.vfa
    port map (nxpcxvm => lcndwzg, enwbppf => eroaknqhl, iqybpqeuth => pkdwa);
end fsk;



-- Seed after: 4604485307466049127,5472058987609252853
