-- Seed: 2832434530916349755,13857275728440271305

entity brx is
  port (jtgyruxvh : out time_vector(3 downto 4); mr : out boolean; yglhxzkpj : in bit_vector(2 to 2); cdl : inout string(5 downto 2));
end brx;

architecture wsytcnxpb of brx is
  
begin
  -- Single-driven assignments
  cdl <= ('p', 'k', 'v', 'q');
  jtgyruxvh <= (others => 0 ns);
  mr <= TRUE;
end wsytcnxpb;

entity xihp is
  port (dy : in real; xe : buffer time);
end xihp;

architecture njvarkvs of xihp is
  signal hrdhetuhsr : string(5 downto 2);
  signal iajtwfntov : boolean;
  signal i : time_vector(3 downto 4);
  signal zclbrxhht : string(5 downto 2);
  signal rsxtlck : boolean;
  signal m : time_vector(3 downto 4);
  signal umkhxq : string(5 downto 2);
  signal fihxcv : bit_vector(2 to 2);
  signal oyhza : boolean;
  signal godndyrj : time_vector(3 downto 4);
begin
  vsmj : entity work.brx
    port map (jtgyruxvh => godndyrj, mr => oyhza, yglhxzkpj => fihxcv, cdl => umkhxq);
  bdnpqodobi : entity work.brx
    port map (jtgyruxvh => m, mr => rsxtlck, yglhxzkpj => fihxcv, cdl => zclbrxhht);
  cwtf : entity work.brx
    port map (jtgyruxvh => i, mr => iajtwfntov, yglhxzkpj => fihxcv, cdl => hrdhetuhsr);
  
  -- Single-driven assignments
  xe <= 8#4# ns;
  fihxcv <= (others => '1');
end njvarkvs;



-- Seed after: 14526913843515662303,13857275728440271305
