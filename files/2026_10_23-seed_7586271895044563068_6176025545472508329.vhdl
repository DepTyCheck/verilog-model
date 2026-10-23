-- Seed: 7586271895044563068,6176025545472508329

library ieee;
use ieee.std_logic_1164.all;

entity ygunvdx is
  port (ica : out std_logic_vector(3 to 3); yu : out std_logic_vector(1 to 3); phdvrxcmhs : out time_vector(4 to 0); hdh : out real);
end ygunvdx;

architecture iiw of ygunvdx is
  
begin
  -- Single-driven assignments
  hdh <= 8#0070.4_0#;
end iiw;

entity ugzodc is
  port (m : inout real);
end ugzodc;

library ieee;
use ieee.std_logic_1164.all;

architecture zshzzca of ugzodc is
  signal nlg : real;
  signal knfvphvybk : time_vector(4 to 0);
  signal kss : std_logic_vector(3 to 3);
  signal bkx : time_vector(4 to 0);
  signal q : std_logic_vector(3 to 3);
  signal brqujqt : real;
  signal usiuqbt : time_vector(4 to 0);
  signal gtbvofewj : std_logic_vector(3 to 3);
  signal osje : real;
  signal ek : time_vector(4 to 0);
  signal wemf : std_logic_vector(1 to 3);
  signal dmqhqu : std_logic_vector(3 to 3);
begin
  lex : entity work.ygunvdx
    port map (ica => dmqhqu, yu => wemf, phdvrxcmhs => ek, hdh => osje);
  er : entity work.ygunvdx
    port map (ica => gtbvofewj, yu => wemf, phdvrxcmhs => usiuqbt, hdh => brqujqt);
  wqxff : entity work.ygunvdx
    port map (ica => q, yu => wemf, phdvrxcmhs => bkx, hdh => m);
  lfle : entity work.ygunvdx
    port map (ica => kss, yu => wemf, phdvrxcmhs => knfvphvybk, hdh => nlg);
  
  -- Multi-driven assignments
  q <= gtbvofewj;
end zshzzca;



-- Seed after: 11962725439960950041,6176025545472508329
