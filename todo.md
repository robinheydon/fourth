- Immediate Mode UI
  - fade out objects that are not 'refreshed'
  - Text (wrapping / formatting)
  - child gaps
  - horizontal alignment - left, center, right
  - vertical alignment - top, center, bottom
  - Expand to fit
  - Shrink to fit
  - CheckButton
  - RadioButton
  - Icons
  - InputText
  - VerticalScrollBar
  - HorizontalScrollBar
  - themes

- Map
  - draw nodes
    - node fragment shader 
  - draw links
    - link fragment shader 
  - move nodes
  - adjust links
  - curved links
  - draw junctions
  - adjust junctions
  - draw people walking
  - draw people with carts
  - buildings
  - zoom to node / building / link
  - Road Sections
    - create road cross section window 
      - select from list of pre-existing road cross sections
      - create new road cross section
      - change a cross section segment type
      - change a cross section segment width
      - add cross section segment
      - delete cross section segment

- ECS
  - save to binary file
  - load from binary file
  - (low) create webserver to expose state of ECS
  - (low) parallel execution

- Gameplay
  - zone roads
  - create buildings
  - create people
  - populate buildings
  - create companies
  - just add an economy !!!
  - Trees
    - plant trees
    - grow trees
    - harvest trees -> trunks / branches / leaves
    - cut branches -> planks / posts / sawdust
  - Education
    - skills
    - schools
    - job learnt skills
  - Health
    - activity level
    - food quality
    - water quality
    - waste management
  - Domesticated Animals
    - wool (sheep)
    - feathers (duck)
    - meat (sheep / pigs / goat / cow / horse / chicken / duck)
    - milk (cow / goat / sheep)
    - eggs (chicken / duck)
  - Wild Animals
    - fox
    - bear
    - dear
    - rabbit

- Graphics
  - Models
  - Instanced draw

- Platforms
  - Linux
    - x11 OpenGL
      - Window Icon
      - Window Title

  - Windows
    - Vulkan
    - DirectX

  - Linux Wayland
    - x11 Vulkan

- Hotreload
  - Linux
