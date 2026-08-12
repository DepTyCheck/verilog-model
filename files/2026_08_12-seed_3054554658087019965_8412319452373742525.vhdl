-- Seed: 3054554658087019965,8412319452373742525

library ieee;
use ieee.std_logic_1164.all;

entity untrdi is
  port (rwouzls : buffer std_logic_vector(0 downto 1); ynlg : linkage std_logic_vector(4 to 0));
end untrdi;

architecture dej of untrdi is
  
begin
  -- Multi-driven assignments
  rwouzls <= rwouzls;
  rwouzls <= (others => '0');
end dej;

entity lcewrr is
  port (nak : inout integer; jzrvh : out character; xjaeci : buffer integer_vector(1 to 4));
end lcewrr;

library ieee;
use ieee.std_logic_1164.all;

architecture q of lcewrr is
  signal wgdfd : std_logic_vector(4 to 0);
  signal ksczwteiz : std_logic_vector(0 downto 1);
begin
  ertwlrdr : entity work.untrdi
    port map (rwouzls => ksczwteiz, ynlg => wgdfd);
  
  -- Single-driven assignments
  xjaeci <= (16#D#, 8#114#, 8#111#, 3);
  nak <= 8#0037#;
  jzrvh <= jzrvh;
  
  -- Multi-driven assignments
  wgdfd <= ksczwteiz;
  ksczwteiz <= (others => '0');
  ksczwteiz <= ksczwteiz;
end q;

entity speizxu is
  port (ubqhggyli : linkage real; bx : out integer; enqf : in real_vector(2 downto 3); rloqvmtn : linkage integer);
end speizxu;

library ieee;
use ieee.std_logic_1164.all;

architecture vcp of speizxu is
  signal oqvbf : std_logic_vector(0 downto 1);
  signal prdxbujqmf : std_logic_vector(0 downto 1);
  signal coiujdqpu : std_logic_vector(4 to 0);
begin
  xvo : entity work.untrdi
    port map (rwouzls => coiujdqpu, ynlg => coiujdqpu);
  wxswzamjz : entity work.untrdi
    port map (rwouzls => prdxbujqmf, ynlg => coiujdqpu);
  qwaskl : entity work.untrdi
    port map (rwouzls => oqvbf, ynlg => coiujdqpu);
  
  -- Single-driven assignments
  bx <= bx;
end vcp;



-- Seed after: 5476849177436659521,8412319452373742525
