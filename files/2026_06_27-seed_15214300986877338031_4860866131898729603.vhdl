-- Seed: 15214300986877338031,4860866131898729603

entity zgiwku is
  port (vctq : linkage time; bjgsbhllm : inout string(2 to 5); ptabe : out bit_vector(0 to 0));
end zgiwku;

architecture jhlewrt of zgiwku is
  
begin
  -- Single-driven assignments
  bjgsbhllm <= ('u', 'n', 'a', 'i');
  ptabe <= (others => '0');
end jhlewrt;

entity etvc is
  port (kpajkzyyn : inout character; vcw : out boolean_vector(2 downto 1));
end etvc;

architecture ebajpp of etvc is
  signal tojoipzj : bit_vector(0 to 0);
  signal xvow : string(2 to 5);
  signal dpxh : time;
  signal kdjbfqnjry : bit_vector(0 to 0);
  signal bsw : string(2 to 5);
  signal zkajx : time;
  signal mcsob : bit_vector(0 to 0);
  signal bvkovxmjpj : string(2 to 5);
  signal npmgswua : time;
  signal xdzygvcx : bit_vector(0 to 0);
  signal p : string(2 to 5);
  signal tknrpnkss : time;
begin
  nwzywguk : entity work.zgiwku
    port map (vctq => tknrpnkss, bjgsbhllm => p, ptabe => xdzygvcx);
  cegizp : entity work.zgiwku
    port map (vctq => npmgswua, bjgsbhllm => bvkovxmjpj, ptabe => mcsob);
  gh : entity work.zgiwku
    port map (vctq => zkajx, bjgsbhllm => bsw, ptabe => kdjbfqnjry);
  darung : entity work.zgiwku
    port map (vctq => dpxh, bjgsbhllm => xvow, ptabe => tojoipzj);
end ebajpp;

library ieee;
use ieee.std_logic_1164.all;

entity uarwwjug is
  port (awdsh : inout std_logic_vector(2 downto 3); arqkkd : in integer);
end uarwwjug;

architecture inujurmrr of uarwwjug is
  signal aklwngh : boolean_vector(2 downto 1);
  signal ipmv : character;
  signal wqtq : bit_vector(0 to 0);
  signal tqlkpibgf : string(2 to 5);
  signal tiacstdzwy : time;
  signal fqqzav : bit_vector(0 to 0);
  signal dnb : string(2 to 5);
  signal qy : time;
begin
  nrtchl : entity work.zgiwku
    port map (vctq => qy, bjgsbhllm => dnb, ptabe => fqqzav);
  cfudoltsch : entity work.zgiwku
    port map (vctq => tiacstdzwy, bjgsbhllm => tqlkpibgf, ptabe => wqtq);
  zvtpslrw : entity work.etvc
    port map (kpajkzyyn => ipmv, vcw => aklwngh);
  
  -- Multi-driven assignments
  awdsh <= "";
  awdsh <= "";
  awdsh <= (others => '0');
end inujurmrr;



-- Seed after: 2682337814365784732,4860866131898729603
