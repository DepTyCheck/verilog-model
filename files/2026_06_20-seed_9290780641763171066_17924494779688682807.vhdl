-- Seed: 9290780641763171066,17924494779688682807

library ieee;
use ieee.std_logic_1164.all;

entity xhao is
  port (ycbzr : out boolean_vector(4 to 3); zghfya : inout std_logic_vector(0 downto 1); hwpoze : in real; hqm : out real);
end xhao;

architecture q of xhao is
  
begin
  -- Single-driven assignments
  hqm <= 021.2;
  
  -- Multi-driven assignments
  zghfya <= (others => '0');
end q;

entity iweltg is
  port (xqltkopdg : in real_vector(0 downto 1));
end iweltg;

library ieee;
use ieee.std_logic_1164.all;

architecture ililgujs of iweltg is
  signal x : real;
  signal xxxsjmnkv : real;
  signal pqkyosoa : boolean_vector(4 to 3);
  signal cawetvi : real;
  signal ypfdhalvk : std_logic_vector(0 downto 1);
  signal c : boolean_vector(4 to 3);
  signal klhdndx : real;
  signal xrbm : std_logic_vector(0 downto 1);
  signal jml : boolean_vector(4 to 3);
  signal xlhlpe : real;
  signal zske : std_logic_vector(0 downto 1);
  signal bi : boolean_vector(4 to 3);
begin
  zpyay : entity work.xhao
    port map (ycbzr => bi, zghfya => zske, hwpoze => xlhlpe, hqm => xlhlpe);
  ftbghqliyx : entity work.xhao
    port map (ycbzr => jml, zghfya => xrbm, hwpoze => xlhlpe, hqm => klhdndx);
  hoskypwsn : entity work.xhao
    port map (ycbzr => c, zghfya => ypfdhalvk, hwpoze => cawetvi, hqm => cawetvi);
  ipuocaeim : entity work.xhao
    port map (ycbzr => pqkyosoa, zghfya => zske, hwpoze => xxxsjmnkv, hqm => x);
  
  -- Single-driven assignments
  xxxsjmnkv <= 2#0_1_1.00#;
end ililgujs;

entity cgxz is
  port (zumipepm : out time_vector(0 to 4); pglti : linkage integer; zbg : inout bit_vector(1 downto 4));
end cgxz;

library ieee;
use ieee.std_logic_1164.all;

architecture jszkaqhz of cgxz is
  signal j : std_logic_vector(0 downto 1);
  signal ndwpvbxtoq : boolean_vector(4 to 3);
  signal jsa : real_vector(0 downto 1);
  signal mjurnpmitm : real;
  signal w : boolean_vector(4 to 3);
  signal azjeka : real;
  signal frnoq : real;
  signal dm : std_logic_vector(0 downto 1);
  signal jsygfiw : boolean_vector(4 to 3);
begin
  s : entity work.xhao
    port map (ycbzr => jsygfiw, zghfya => dm, hwpoze => frnoq, hqm => azjeka);
  rqu : entity work.xhao
    port map (ycbzr => w, zghfya => dm, hwpoze => frnoq, hqm => mjurnpmitm);
  k : entity work.iweltg
    port map (xqltkopdg => jsa);
  uwewheid : entity work.xhao
    port map (ycbzr => ndwpvbxtoq, zghfya => j, hwpoze => azjeka, hqm => frnoq);
  
  -- Single-driven assignments
  jsa <= (others => 0.0);
  zbg <= (others => '0');
  
  -- Multi-driven assignments
  dm <= (others => '0');
end jszkaqhz;



-- Seed after: 4326234306325682364,17924494779688682807
