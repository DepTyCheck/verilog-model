-- Seed: 13258326416980983697,6697892553037813751

library ieee;
use ieee.std_logic_1164.all;

entity hjxfpdq is
  port ( zbxhmh : linkage std_logic_vector(4 downto 3)
  ; dsxzqsw : buffer std_logic_vector(4 downto 4)
  ; xpqixx : in real
  ; myf : out real_vector(0 downto 0)
  );
end hjxfpdq;

architecture uimoflzrdo of hjxfpdq is
  
begin
  -- Single-driven assignments
  myf <= (others => 024.3_3_0_2);
  
  -- Multi-driven assignments
  dsxzqsw <= (others => '-');
  dsxzqsw <= (others => 'X');
  dsxzqsw <= "U";
  dsxzqsw <= (others => 'W');
end uimoflzrdo;

library ieee;
use ieee.std_logic_1164.all;

entity emxp is
  port (kadrdco : buffer bit; skkynedyz : in std_logic);
end emxp;

library ieee;
use ieee.std_logic_1164.all;

architecture jidrdeauw of emxp is
  signal biuwemmuk : real_vector(0 downto 0);
  signal plv : real;
  signal pifwsm : std_logic_vector(4 downto 4);
  signal ouwau : std_logic_vector(4 downto 3);
begin
  ven : entity work.hjxfpdq
    port map (zbxhmh => ouwau, dsxzqsw => pifwsm, xpqixx => plv, myf => biuwemmuk);
end jidrdeauw;

entity sxtwi is
  port (fgz : linkage integer);
end sxtwi;

library ieee;
use ieee.std_logic_1164.all;

architecture vgjloob of sxtwi is
  signal lyjuzuqn : std_logic;
  signal a : bit;
  signal houbmx : std_logic;
  signal nc : bit;
  signal mpnixgdvmo : real_vector(0 downto 0);
  signal yjbdaidie : real;
  signal gyvh : std_logic_vector(4 downto 4);
  signal fujlakks : std_logic_vector(4 downto 3);
begin
  q : entity work.hjxfpdq
    port map (zbxhmh => fujlakks, dsxzqsw => gyvh, xpqixx => yjbdaidie, myf => mpnixgdvmo);
  wo : entity work.emxp
    port map (kadrdco => nc, skkynedyz => houbmx);
  rhjwde : entity work.emxp
    port map (kadrdco => a, skkynedyz => lyjuzuqn);
  
  -- Single-driven assignments
  yjbdaidie <= 2#11.01010#;
  
  -- Multi-driven assignments
  gyvh <= "Z";
  fujlakks <= "LZ";
  fujlakks <= ('U', '0');
  fujlakks <= ('0', 'W');
end vgjloob;

entity papxq is
  port (u : linkage time);
end papxq;

library ieee;
use ieee.std_logic_1164.all;

architecture qzybxjadp of papxq is
  signal t : integer;
  signal bx : real_vector(0 downto 0);
  signal oqtx : real;
  signal ovvjfbyze : std_logic_vector(4 downto 4);
  signal m : std_logic_vector(4 downto 3);
  signal ahtbpseq : std_logic;
  signal jnvwmfbiso : bit;
begin
  s : entity work.emxp
    port map (kadrdco => jnvwmfbiso, skkynedyz => ahtbpseq);
  wpr : entity work.hjxfpdq
    port map (zbxhmh => m, dsxzqsw => ovvjfbyze, xpqixx => oqtx, myf => bx);
  gzrrpbfi : entity work.sxtwi
    port map (fgz => t);
  
  -- Single-driven assignments
  oqtx <= 012.010;
  
  -- Multi-driven assignments
  ovvjfbyze <= (others => '0');
  ahtbpseq <= 'U';
  ahtbpseq <= 'W';
  ovvjfbyze <= "0";
end qzybxjadp;



-- Seed after: 13004032371097365564,6697892553037813751
