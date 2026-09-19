-- Seed: 6892620739557184142,14141408946471626091

entity pejtatkd is
  port (ohejek : out real);
end pejtatkd;

architecture jdny of pejtatkd is
  
begin
  -- Single-driven assignments
  ohejek <= 16#B43.4B7#;
end jdny;

library ieee;
use ieee.std_logic_1164.all;

entity ttnw is
  port (yanxnxa : in std_logic_vector(2 to 0));
end ttnw;

architecture odnqtt of ttnw is
  signal ejjkf : real;
  signal dwdtim : real;
  signal ljycgqpu : real;
  signal yqksz : real;
begin
  sbvyqje : entity work.pejtatkd
    port map (ohejek => yqksz);
  fddomh : entity work.pejtatkd
    port map (ohejek => ljycgqpu);
  t : entity work.pejtatkd
    port map (ohejek => dwdtim);
  kom : entity work.pejtatkd
    port map (ohejek => ejjkf);
end odnqtt;

entity jrof is
  port (cgxklkepd : inout string(1 to 5); yw : buffer integer; eualzk : out real; svmrcirq : buffer real_vector(0 downto 2));
end jrof;

library ieee;
use ieee.std_logic_1164.all;

architecture hghscy of jrof is
  signal kpucqbxnkv : std_logic_vector(2 to 0);
begin
  urzd : entity work.ttnw
    port map (yanxnxa => kpucqbxnkv);
  vqtyli : entity work.pejtatkd
    port map (ohejek => eualzk);
  
  -- Multi-driven assignments
  kpucqbxnkv <= "";
end hghscy;



-- Seed after: 16440751232819381568,14141408946471626091
