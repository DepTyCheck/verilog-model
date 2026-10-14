-- Seed: 9956265780946562848,2287986438313874379

entity xygnlnqdo is
  port (jdsyo : out integer; tvzquyxmvu : linkage bit_vector(2 downto 2); nlpkdigxsz : linkage time; w : buffer real);
end xygnlnqdo;

architecture eonlap of xygnlnqdo is
  
begin
  -- Single-driven assignments
  w <= w;
  jdsyo <= 8#5_3_4_7#;
end eonlap;

entity bznwcoib is
  port (rktzrfym : buffer time);
end bznwcoib;

architecture s of bznwcoib is
  signal xbzjkxglc : real;
  signal dlfhkyiggz : bit_vector(2 downto 2);
  signal xawm : integer;
  signal gvz : real;
  signal kxgb : time;
  signal qgxip : bit_vector(2 downto 2);
  signal wxz : integer;
  signal qyo : real;
  signal bqnlkykfrp : time;
  signal pvifshn : bit_vector(2 downto 2);
  signal ztegfngqw : integer;
begin
  dxsjq : entity work.xygnlnqdo
    port map (jdsyo => ztegfngqw, tvzquyxmvu => pvifshn, nlpkdigxsz => bqnlkykfrp, w => qyo);
  psy : entity work.xygnlnqdo
    port map (jdsyo => wxz, tvzquyxmvu => qgxip, nlpkdigxsz => kxgb, w => gvz);
  gyzvvg : entity work.xygnlnqdo
    port map (jdsyo => xawm, tvzquyxmvu => dlfhkyiggz, nlpkdigxsz => rktzrfym, w => xbzjkxglc);
end s;

library ieee;
use ieee.std_logic_1164.all;

entity es is
  port (pkbn : linkage boolean_vector(1 downto 0); r : inout std_logic; advhhmwz : out std_logic_vector(4 downto 0));
end es;

architecture qgycbugay of es is
  signal ik : real;
  signal qcvxqywjx : time;
  signal vgd : bit_vector(2 downto 2);
  signal fdi : integer;
begin
  ub : entity work.xygnlnqdo
    port map (jdsyo => fdi, tvzquyxmvu => vgd, nlpkdigxsz => qcvxqywjx, w => ik);
end qgycbugay;

entity xkmxsvoqud is
  port (qitz : out time; gnhkrqhvya : out integer; y : out bit_vector(4 to 4); kpjrpxj : buffer integer_vector(3 downto 0));
end xkmxsvoqud;

library ieee;
use ieee.std_logic_1164.all;

architecture xfcwbegs of xkmxsvoqud is
  signal kaqmkztf : real;
  signal pnza : std_logic_vector(4 downto 0);
  signal asgcinc : std_logic;
  signal uqjmcbuya : boolean_vector(1 downto 0);
begin
  ao : entity work.es
    port map (pkbn => uqjmcbuya, r => asgcinc, advhhmwz => pnza);
  nrwk : entity work.xygnlnqdo
    port map (jdsyo => gnhkrqhvya, tvzquyxmvu => y, nlpkdigxsz => qitz, w => kaqmkztf);
  
  -- Single-driven assignments
  kpjrpxj <= kpjrpxj;
  
  -- Multi-driven assignments
  asgcinc <= 'H';
  pnza <= pnza;
  asgcinc <= asgcinc;
end xfcwbegs;



-- Seed after: 4549191079790688522,2287986438313874379
