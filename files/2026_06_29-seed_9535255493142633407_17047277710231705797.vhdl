-- Seed: 9535255493142633407,17047277710231705797

library ieee;
use ieee.std_logic_1164.all;

entity bfdjztrz is
  port (ykndcjitrl : buffer std_logic);
end bfdjztrz;

architecture pal of bfdjztrz is
  
begin
  -- Multi-driven assignments
  ykndcjitrl <= 'Z';
  ykndcjitrl <= '0';
  ykndcjitrl <= 'U';
end pal;

library ieee;
use ieee.std_logic_1164.all;

entity eaxslqd is
  port (vdjyhwzjb : inout integer; akpbma : in integer; ywvd : linkage std_logic_vector(0 to 3); axci : linkage std_logic_vector(2 downto 3));
end eaxslqd;

architecture qzo of eaxslqd is
  
begin
  -- Single-driven assignments
  vdjyhwzjb <= 1_1_2_4;
end qzo;

library ieee;
use ieee.std_logic_1164.all;

entity ljajqm is
  port (zuebqlz : buffer std_logic_vector(2 to 3); moztipan : buffer bit);
end ljajqm;

library ieee;
use ieee.std_logic_1164.all;

architecture kuaebuy of ljajqm is
  signal zji : std_logic_vector(2 downto 3);
  signal msrw : std_logic_vector(0 to 3);
  signal jyipjod : integer;
  signal mgxy : std_logic;
begin
  tjd : entity work.bfdjztrz
    port map (ykndcjitrl => mgxy);
  edretb : entity work.eaxslqd
    port map (vdjyhwzjb => jyipjod, akpbma => jyipjod, ywvd => msrw, axci => zji);
  
  -- Single-driven assignments
  moztipan <= '1';
  
  -- Multi-driven assignments
  mgxy <= 'X';
end kuaebuy;



-- Seed after: 10415519135431918218,17047277710231705797
