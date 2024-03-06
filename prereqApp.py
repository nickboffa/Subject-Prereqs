import kivy 
from kivy.app import App
from kivy.uix.floatlayout import FloatLayout
from kivy.uix.boxlayout import BoxLayout
from kivy.uix.button import Button
from kivy.uix.label import Label
from kivy.uix.textinput import TextInput
from kivy.uix.togglebutton import ToggleButton
from kivy.graphics import Color, Line, Triangle
from kivy.uix.image import Image
from kivy.properties import StringProperty, ListProperty, DictProperty

kivy.require('2.2.1')

import math
from functions import *

G = create_all_graph()

#copied from stack overflow
class WrappedLabel(Label):
    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self.bind(
            width=lambda *x:
            self.setter('text_size')(self, (self.width, None)),
            texture_size=lambda *x: self.setter('height')(self, self.texture_size[1]))
        
class InOutButton(ToggleButton):
    direction = StringProperty("in")

class MyFloatLayout(FloatLayout):
    arrows = ListProperty([])
    highlighted_edges = ListProperty([])
    edge_arrows = DictProperty({})

    def clear_arrows(self):
        for line, triangle in self.arrows:
            self.canvas.before.remove(line)
            self.canvas.before.remove(triangle)
        self.arrows = []

    def get_edge_coords(self, edge):
        px0,py0 = pos[edge[0]]
        px1,py1 = pos[edge[1]]

        x0 = px0*self.size[0] + self.pos[0]
        x1 = px1*self.size[0] + self.pos[0]
        y0 = py0*self.size[1] + self.pos[1]
        y1 = py1*self.size[1] + self.pos[1]

        return x0, y0, x1, y1
    
    def update_arrows(self, instance, value): #float_object, float_object, float_object.size

        self.clear_arrows()

        for edge in H.edges:

            x0, y0, x1, y1 = self.get_edge_coords(edge)
            v = (x1-x0,y1-y0)

            triangle_length = 20 #pixels?
            unit_v = tuple([(1/math.sqrt(v[0]**2+v[1]**2))*component for component in v]) #normalising
            perp_unit_v = (-unit_v[1], unit_v[0])

            p1 = add((x0,y0), scale(v, 0.5)) #midpoint of line
            base_midpoint = add(p1, scale(unit_v, -triangle_length)) #midpoint of the base of the triangle (still on line)

            a = triangle_length/math.cos(math.pi/6) #equilateral triangle as the arrowhead
            p2 = add(base_midpoint, scale(perp_unit_v, a/2))
            p3 = add(base_midpoint, scale(perp_unit_v, -a/2))

            shift = (scale(unit_v, 0.5*triangle_length)) 
            p1 = add(p1, shift)
            p2 = add(p2, shift)
            p3 = add(p3, shift)
                
            self.edge_arrows[edge] = [p1[0],p1[1], p2[0],p2[1], p3[0],p3[1]]

            with self.canvas.before:
                if edge in self.highlighted_edges:
                    Color(0, 1, 0, 1, mode='rgba')
                else:
                    Color(1, 1, 1, 0.2, mode='rgba')

                l = Line(points=[x0,y0,x1,y1], width=2)
                t = Triangle(points = self.edge_arrows[edge])

            self.arrows.append((l,t))

    def highlight_arrows(self, instance): #self, button
        code = instance.text
        highlight_nodes = nx.ancestors(H, code).union(nx.descendants(H,code), {code})
        I = H.subgraph(highlight_nodes)
        
        self.clear_arrows()

        for edge in H.edges:
            
            x0, y0, x1, y1 = self.get_edge_coords(edge)

            with self.canvas.before:
                    if edge in I.edges:
                        Color(0,1,0,1, mode="rgba")

                        if edge not in self.highlighted_edges:
                            self.highlighted_edges.append(edge)
                            
                    else: #so that they turn back to white even after being highlighted
                        Color(1,1,1,0.2, mode="rgba")
                        
                        if edge in self.highlighted_edges:
                            self.highlighted_edges.remove(edge)
                        
                    l = Line(points=[x0,y0,x1,y1], width=2)
                    t = Triangle(points = self.edge_arrows[edge])

                    self.arrows.append((l, t))

# from https://www.tutorialspoint.com/how-to-add-drag-behavior-in-kivy-widget
class DraggableButton(ToggleButton):
   def __init__(self, **kwargs):
       super().__init__(**kwargs)
   # Override the on_touch_down method to detect when the user touches the widget
   def on_touch_down(self, touch):
      if self.collide_point(*touch.pos):
         # If the touch event occurred within the widget's bounds, handle the touch event
         # by setting the widget as the current touch target
         touch.grab(self)
         return True
      return super().on_touch_down(touch)

   # Override the on_touch_move method to track the movement of the user's finger
   def on_touch_move(self, touch):
      if touch.grab_current == self:
         # If the touch event is being handled by our widget, update the widget's position
         self.pos = (self.pos[0] + touch.dx, self.pos[1] + touch.dy)

   # Override the on_touch_up method to update the widget's position when the touch event ends
   def on_touch_up(self, touch):
      if touch.grab_current == self:
         # If the touch event is being handled by our widget, release the widget as the current
         # touch target and handle the touch event
         touch.ungrab(self)
         return True
      return super().on_touch_up(touch)

class MyApp(App):
    def build(self):
        self.b = BoxLayout(orientation ='vertical')
        self.f = MyFloatLayout(size_hint=(1,1))

        self.b2 = BoxLayout(orientation='horizontal', size_hint=(1,0.1))
        self.target_course = TextInput(font_size = 50, size_hint=(1,1), multiline=False)
        enter = Button(text='Enter', on_press=self.submit, size_hint=(0.25,1))
        
        self.btn = InOutButton(text="In", group="direction", background_color=[1,0,1,1],
                               on_press=self.set_direction)

        self.b2.add_widget(self.target_course)
        self.b2.add_widget(self.btn)
        self.b2.add_widget(enter)

        self.blurb = Label(text='', size_hint=(1, 0.2))

        self.b.add_widget(self.b2)
        self.b.add_widget(self.f)
        self.b.add_widget(self.blurb)

        return self.b

    def submit(self, button):
        target = self.target_course.text.upper() 
        if target in all_codes:
            self.b.remove_widget(self.f)
            self.f = MyFloatLayout(size_hint=(1,1))

            global H, pos, color_map
            H, pos, color_map = get_layout_info(target, G, direction=self.btn.direction)

            for code in pos.keys():
                x,y= pos[code]
                B = ToggleButton(text=code, pos_hint={'center_x':x, 'center_y':y}, 
                        size_hint=(0.04,0.04), background_color=color_map[code],
                        group='courses', on_press=self.display)

                self.f.add_widget(B)
            
            self.f.bind(size=self.f.update_arrows)

            self.b.add_widget(self.f, index=1)

    def display(self, button):
        self.show_blurb(button)
        self.f.highlight_arrows(button)

    def show_blurb(self, button):
        self.b.remove_widget(self.blurb)
        self.blurb = WrappedLabel(text=f'{button.text}: {names[button.text]} \n {blurbs[button.text]}', 
                                  size_hint=(1, 0.2))

        self.b.add_widget(self.blurb, index=0)

    def set_direction(self, button):
        if button.state == 'down':
            button.text = 'Out'
            button.background_color = [1,1,0,1]
        else:
            button.text = 'In'
            button.background_color = [1,0,1,1]
        
        button.direction = button.text.lower()

    


if __name__ == '__main__':
    MyApp().run()



#jpns6113