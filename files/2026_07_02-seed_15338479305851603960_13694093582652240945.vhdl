-- Seed: 15338479305851603960,13694093582652240945

entity iymq is
  port (l : buffer string(2 to 4));
end iymq;

architecture cgvnhyinvx of iymq is
  
begin
  -- Single-driven assignments
  l <= ('n', 'i', 'z');
end cgvnhyinvx;

entity xibkkxta is
  port (y : out bit_vector(2 downto 1); bjnlrdsbav : out time);
end xibkkxta;

architecture ydkudw of xibkkxta is
  signal xfcqgng : string(2 to 4);
begin
  dotdpec : entity work.iymq
    port map (l => xfcqgng);
  
  -- Single-driven assignments
  bjnlrdsbav <= 8#06.32527# ns;
  y <= ('0', '1');
end ydkudw;



-- Seed after: 4874469926792469275,13694093582652240945
