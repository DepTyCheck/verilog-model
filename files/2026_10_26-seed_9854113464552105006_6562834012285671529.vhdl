-- Seed: 9854113464552105006,6562834012285671529

entity tqzwxa is
  port (x : in boolean_vector(2 downto 2); fccluywc : out bit_vector(0 downto 4));
end tqzwxa;

architecture oxrogedpfr of tqzwxa is
  
begin
  
end oxrogedpfr;

entity butfuvwe is
  port (cyhuq : in integer; tat : buffer integer; x : inout time);
end butfuvwe;

architecture vuyy of butfuvwe is
  signal mgkfnw : bit_vector(0 downto 4);
  signal svxz : bit_vector(0 downto 4);
  signal lfwwp : boolean_vector(2 downto 2);
  signal yelkvhk : bit_vector(0 downto 4);
  signal tgimbewj : boolean_vector(2 downto 2);
  signal a : bit_vector(0 downto 4);
  signal qlbf : boolean_vector(2 downto 2);
begin
  cdx : entity work.tqzwxa
    port map (x => qlbf, fccluywc => a);
  tggt : entity work.tqzwxa
    port map (x => tgimbewj, fccluywc => yelkvhk);
  escphm : entity work.tqzwxa
    port map (x => lfwwp, fccluywc => svxz);
  i : entity work.tqzwxa
    port map (x => qlbf, fccluywc => mgkfnw);
  
  -- Single-driven assignments
  qlbf <= (others => FALSE);
  x <= 0 hr;
  lfwwp <= (others => FALSE);
  tgimbewj <= qlbf;
end vuyy;



-- Seed after: 2689925960770934500,6562834012285671529
