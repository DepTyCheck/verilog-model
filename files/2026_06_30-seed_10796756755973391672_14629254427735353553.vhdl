-- Seed: 10796756755973391672,14629254427735353553

library ieee;
use ieee.std_logic_1164.all;

entity rwxfagz is
  port (zonlautvo : in boolean; y : out time_vector(4 downto 1); xgojbqei : buffer bit; dneewrbx : buffer std_logic);
end rwxfagz;

architecture utsq of rwxfagz is
  
begin
  -- Single-driven assignments
  y <= (0 ns, 2#1# ps, 8#17# us, 1 min);
  xgojbqei <= '0';
end utsq;

entity rghzy is
  port (ajjocqzhk : out real; cyxrab : in bit);
end rghzy;

library ieee;
use ieee.std_logic_1164.all;

architecture rvtmkj of rghzy is
  signal bh : std_logic;
  signal vq : bit;
  signal xgqo : time_vector(4 downto 1);
  signal zaayfowju : boolean;
  signal wcc : std_logic;
  signal tmqsxiug : bit;
  signal vdkndzdu : time_vector(4 downto 1);
  signal qimvic : std_logic;
  signal cwssu : bit;
  signal dqzxqjus : time_vector(4 downto 1);
  signal doef : boolean;
begin
  eosjstibqs : entity work.rwxfagz
    port map (zonlautvo => doef, y => dqzxqjus, xgojbqei => cwssu, dneewrbx => qimvic);
  sy : entity work.rwxfagz
    port map (zonlautvo => doef, y => vdkndzdu, xgojbqei => tmqsxiug, dneewrbx => wcc);
  lcgh : entity work.rwxfagz
    port map (zonlautvo => zaayfowju, y => xgqo, xgojbqei => vq, dneewrbx => bh);
  
  -- Single-driven assignments
  ajjocqzhk <= 0432.2;
  zaayfowju <= FALSE;
  doef <= TRUE;
  
  -- Multi-driven assignments
  bh <= '-';
  wcc <= '0';
  wcc <= '1';
  qimvic <= '1';
end rvtmkj;



-- Seed after: 12391445938589635082,14629254427735353553
