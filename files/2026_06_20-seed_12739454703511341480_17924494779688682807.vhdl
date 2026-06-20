-- Seed: 12739454703511341480,17924494779688682807

entity qx is
  port (zfv : in bit; zmahtqinv : buffer boolean);
end qx;

architecture oautpa of qx is
  
begin
  -- Single-driven assignments
  zmahtqinv <= FALSE;
end oautpa;

entity aho is
  port (safdgwjz : inout boolean);
end aho;

architecture fu of aho is
  
begin
  -- Single-driven assignments
  safdgwjz <= FALSE;
end fu;

entity r is
  port (bl : buffer time; exyyw : inout integer; jmbwyig : linkage bit_vector(0 downto 4); nfh : inout real_vector(0 downto 3));
end r;

architecture zsvrcf of r is
  signal ofkzzxod : boolean;
  signal zsio : boolean;
  signal v : boolean;
  signal pfmtxv : bit;
begin
  xkbbh : entity work.qx
    port map (zfv => pfmtxv, zmahtqinv => v);
  jmkgxs : entity work.aho
    port map (safdgwjz => zsio);
  rcmrggeh : entity work.qx
    port map (zfv => pfmtxv, zmahtqinv => ofkzzxod);
  
  -- Single-driven assignments
  bl <= 4.4 ns;
  pfmtxv <= '0';
  exyyw <= 16#6_2_5_7#;
  nfh <= (others => 0.0);
end zsvrcf;



-- Seed after: 3823766978819027597,17924494779688682807
