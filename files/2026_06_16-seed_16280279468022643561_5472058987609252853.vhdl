-- Seed: 16280279468022643561,5472058987609252853

library ieee;
use ieee.std_logic_1164.all;

entity qjfdtu is
  port ( ekiqsxwpc : in real_vector(0 downto 0)
  ; qcbnnx : buffer std_logic_vector(0 downto 2)
  ; thqhvyrs : in std_logic_vector(4 to 2)
  ; zxlqsbsybu : buffer std_logic_vector(4 to 3)
  );
end qjfdtu;

architecture cpufftqeig of qjfdtu is
  
begin
  -- Multi-driven assignments
  zxlqsbsybu <= (others => '0');
end cpufftqeig;

library ieee;
use ieee.std_logic_1164.all;

entity v is
  port (mpxuiudfw : out integer; sdkcntzmmw : out std_logic_vector(0 to 0); frfyuligs : inout integer);
end v;

architecture fitdyyjz of v is
  
begin
  -- Single-driven assignments
  frfyuligs <= 8#4_6_2#;
  mpxuiudfw <= 2#0#;
  
  -- Multi-driven assignments
  sdkcntzmmw <= (others => '1');
  sdkcntzmmw <= (others => 'H');
  sdkcntzmmw <= "U";
end fitdyyjz;

entity aute is
  port (acngkbpt : buffer time);
end aute;

library ieee;
use ieee.std_logic_1164.all;

architecture ntisolhvz of aute is
  signal odemxqu : std_logic_vector(4 to 3);
  signal uuu : std_logic_vector(4 to 2);
  signal j : std_logic_vector(4 to 3);
  signal wowdge : std_logic_vector(4 to 2);
  signal prrjeqg : std_logic_vector(0 downto 2);
  signal ujtbtiuds : real_vector(0 downto 0);
begin
  roelx : entity work.qjfdtu
    port map (ekiqsxwpc => ujtbtiuds, qcbnnx => prrjeqg, thqhvyrs => wowdge, zxlqsbsybu => j);
  qegdv : entity work.qjfdtu
    port map (ekiqsxwpc => ujtbtiuds, qcbnnx => prrjeqg, thqhvyrs => uuu, zxlqsbsybu => odemxqu);
  
  -- Single-driven assignments
  acngkbpt <= 1 hr;
  ujtbtiuds <= (others => 3_2_2.4);
  
  -- Multi-driven assignments
  uuu <= (others => '0');
  uuu <= "";
end ntisolhvz;

entity ksnirp is
  port (xkwahpxqfa : out integer);
end ksnirp;

library ieee;
use ieee.std_logic_1164.all;

architecture acono of ksnirp is
  signal yfnjsbwzs : std_logic_vector(4 to 3);
  signal tpvi : real_vector(0 downto 0);
  signal mbppdoe : time;
  signal vjnktzzc : std_logic_vector(4 to 3);
  signal zazriup : std_logic_vector(0 downto 2);
  signal jdj : std_logic_vector(4 to 2);
  signal ojpjceg : std_logic_vector(4 to 2);
  signal jlgxmh : std_logic_vector(0 downto 2);
  signal ny : real_vector(0 downto 0);
begin
  p : entity work.qjfdtu
    port map (ekiqsxwpc => ny, qcbnnx => jlgxmh, thqhvyrs => ojpjceg, zxlqsbsybu => jdj);
  zk : entity work.qjfdtu
    port map (ekiqsxwpc => ny, qcbnnx => jdj, thqhvyrs => zazriup, zxlqsbsybu => vjnktzzc);
  joka : entity work.aute
    port map (acngkbpt => mbppdoe);
  wpha : entity work.qjfdtu
    port map (ekiqsxwpc => tpvi, qcbnnx => zazriup, thqhvyrs => jdj, zxlqsbsybu => yfnjsbwzs);
end acono;



-- Seed after: 10125140674547128444,5472058987609252853
