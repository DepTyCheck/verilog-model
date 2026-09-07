-- Seed: 1587803691026167815,12269339630485015285

library ieee;
use ieee.std_logic_1164.all;

entity bap is
  port (zsyuqkyil : linkage boolean_vector(3 downto 1); imfi : buffer real; ceedqqrdli : buffer real; krnhovhnp : out std_logic_vector(3 downto 4));
end bap;

architecture hbsout of bap is
  
begin
  -- Single-driven assignments
  ceedqqrdli <= 2#0_0_1_0_0.100#;
  imfi <= 43.303;
  
  -- Multi-driven assignments
  krnhovhnp <= krnhovhnp;
  krnhovhnp <= (others => '0');
  krnhovhnp <= (others => '0');
end hbsout;

library ieee;
use ieee.std_logic_1164.all;

entity sipo is
  port (vyk : linkage std_logic; xfdvmueyb : inout std_logic_vector(4 downto 0));
end sipo;

library ieee;
use ieee.std_logic_1164.all;

architecture ki of sipo is
  signal xlb : real;
  signal ftcy : real;
  signal n : boolean_vector(3 downto 1);
  signal oflkzapfdh : std_logic_vector(3 downto 4);
  signal fvdvurjlhu : real;
  signal ibhkislb : real;
  signal miulfhmqqa : boolean_vector(3 downto 1);
  signal hdpeuivzh : real;
  signal cbfjbrp : real;
  signal smsbmkvyug : boolean_vector(3 downto 1);
  signal pzsykca : std_logic_vector(3 downto 4);
  signal xfurjp : real;
  signal pwopnhia : real;
  signal miops : boolean_vector(3 downto 1);
begin
  vi : entity work.bap
    port map (zsyuqkyil => miops, imfi => pwopnhia, ceedqqrdli => xfurjp, krnhovhnp => pzsykca);
  plzfsep : entity work.bap
    port map (zsyuqkyil => smsbmkvyug, imfi => cbfjbrp, ceedqqrdli => hdpeuivzh, krnhovhnp => pzsykca);
  g : entity work.bap
    port map (zsyuqkyil => miulfhmqqa, imfi => ibhkislb, ceedqqrdli => fvdvurjlhu, krnhovhnp => oflkzapfdh);
  bocm : entity work.bap
    port map (zsyuqkyil => n, imfi => ftcy, ceedqqrdli => xlb, krnhovhnp => pzsykca);
  
  -- Multi-driven assignments
  pzsykca <= pzsykca;
  pzsykca <= pzsykca;
  xfdvmueyb <= xfdvmueyb;
end ki;



-- Seed after: 10552133764114516212,12269339630485015285
