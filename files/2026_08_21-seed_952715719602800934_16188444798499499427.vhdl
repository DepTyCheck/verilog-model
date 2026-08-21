-- Seed: 952715719602800934,16188444798499499427

entity iffztv is
  port (l : out string(4 to 4); ogbtholdb : buffer character; wjrv : inout string(2 to 2));
end iffztv;

architecture p of iffztv is
  
begin
  
end p;

library ieee;
use ieee.std_logic_1164.all;

entity tlead is
  port (npsvqui : inout std_logic_vector(3 to 1); whtjcbsl : linkage boolean_vector(3 to 1); whyyme : out boolean; afyxkumy : out std_logic);
end tlead;

architecture wysggnodzr of tlead is
  signal s : string(2 to 2);
  signal jyv : character;
  signal tfsps : string(4 to 4);
  signal jmtico : string(2 to 2);
  signal hxzzffzk : character;
  signal rolhvuhxd : string(4 to 4);
begin
  inyv : entity work.iffztv
    port map (l => rolhvuhxd, ogbtholdb => hxzzffzk, wjrv => jmtico);
  bjxcwho : entity work.iffztv
    port map (l => tfsps, ogbtholdb => jyv, wjrv => s);
  
  -- Single-driven assignments
  whyyme <= TRUE;
  
  -- Multi-driven assignments
  afyxkumy <= '1';
end wysggnodzr;

entity rwcfwgvl is
  port (iyjz : inout real);
end rwcfwgvl;

library ieee;
use ieee.std_logic_1164.all;

architecture fwllwnszm of rwcfwgvl is
  signal ytpcsixhwy : string(2 to 2);
  signal dauv : character;
  signal znovzasqor : string(4 to 4);
  signal ze : std_logic;
  signal nvmye : boolean;
  signal uxtarv : boolean_vector(3 to 1);
  signal jcpnkzf : std_logic_vector(3 to 1);
begin
  o : entity work.tlead
    port map (npsvqui => jcpnkzf, whtjcbsl => uxtarv, whyyme => nvmye, afyxkumy => ze);
  gjmzqtirk : entity work.iffztv
    port map (l => znovzasqor, ogbtholdb => dauv, wjrv => ytpcsixhwy);
  
  -- Single-driven assignments
  iyjz <= 3_2_1.1110;
  
  -- Multi-driven assignments
  jcpnkzf <= (others => '0');
end fwllwnszm;



-- Seed after: 17028003445568301314,16188444798499499427
